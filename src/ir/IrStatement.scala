package ir

import antlr.collections.AST
import edu.mit.compilers.grammar.{ DecafParser, DecafParserTokenTypes }
import util.{ Helper, ErrText }

abstract class IrStatement(implicit ref: AST) {
  def check(g: Global): (Global, List[ErrMsg])
  def refract(n: NameNote): (List[lir.Statement], NameNote)
}

case class IrStatementFieldDecl(kind: VariableType, ids: List[IrVariable])(implicit ref: AST) extends IrStatement {
  def check(g: Global): (Global, List[ErrMsg]) =
    ((g, List[ErrMsg]()) /: (ids)) {case ((oldg, olderrs), v) => v match {
      case IrVariableScalar(id) => oldg.getIdInner(id) match {
        case Some(_) => (oldg, olderrs :+ ErrMsg(ErrText.duplicatefield(id)))
        case None => (oldg.addSymbol(id -> Scalar(kind)), olderrs)
      }
      case IrVariableVector(id, size) => (oldg.getIdInner(id), size) match {
        case (_, Some(i)) if i <= 0 => (oldg, olderrs :+ ErrMsg(ErrText.badarraysize(id)))
        case (_, None) => (oldg, olderrs :+ ErrMsg(ErrText.intoverflow(Helper.getnthChild(v.ref, 2).getText())))
        case (Some(_), _) => (oldg, olderrs :+ ErrMsg(ErrText.duplicatefield(id)))
        case (None, Some(s)) => (oldg.addSymbol(id -> Vector(kind, s)), olderrs)
      }
    }}
  def refract(n: NameNote) = {
    val newNameNote = (n /: ids) ({ (ns, v) => v match {
      case IrVariableScalar(id) => ns.addName(id, lir.VariableScalar(kind))._2
      case IrVariableVector(id, Some(s)) => ns.addName(id, lir.VariableVector(kind, s))._2
    }})
    (List[lir.Statement](), newNameNote)
  }
}
case class IrStatementAssign(op: AssignType, location: IrLocation, value: IrExpr)(implicit ref: AST) extends IrStatement {
  def check(g: Global): (Global, List[ErrMsg]) = location.getType(g) match {
    case (_, Some(e)) => (g, List(e))
    case (Some(kind), _) => value.getType(g) match {
      case (_, Some(e)) => (g, List(e))
      case (vkind, _) => (g, if (Some(kind) == vkind || vkind == Some(VariableCallout)) 
        if (kind == VariableBoolean && op != AssignEQ) List(ErrMsg(ErrText.assignnonint(location.id))) else List[ErrMsg]()
      else List(ErrMsg(ErrText.assignmentmismatch(location.id))))
    }
    // Never reach here. Just to ignore warning messsage
    case (None, None) => (g, List[ErrMsg]())
  }
  def refract(n: NameNote) = {
    val (ss1, v, n1) = value.refract(n)
    location match {
      case IrLocationScalar(id) => (ss1 :+ lir.StmtAssign(n.getName(id), op, v), n1)
      case IrLocationVector(id, index) => {
        val (ss2, idx, n2) = index.refract(n1)
        (ss1 ++ ss2 :+ lir.StmtAssignArray(n.getName(id), idx, op, v, n2.getSizeArray(id)), n2)
      }
  }}
}
case class IrStatementCall(id: String, params: List[IrParam])(implicit ref: AST) extends IrStatement {
  def check(g: Global): (Global, List[ErrMsg]) = g.getId(id) match {
    case Some(Callout()) => (g, params.map(_.getType(g)).map(_._2).flatten)
    case Some(Method(_, xs)) => {
      val tps = params map (_.getType(g))
      val rps = tps.map(_._1).flatten
      val res = tps.map(_._2).flatten
      (g, (if (IrParam.compareParams(rps, xs)) List[ErrMsg]() else List(ErrMsg(ErrText.methodparamsmissing(id)))) ++ res)
    }
    case None => (g, List(ErrMsg(ErrText.undefinedmethod(id))))
    case _ => (g, List(ErrMsg(ErrText.methodmismatch(id))))
  }
  def refract(n: NameNote) = {
    val (ss, pp, nn) = ((List[lir.Statement](), List[lir.Param](), n) /: params) ({ case ((f_ss, f_pp, f_nn), param) =>
      val (r_ss, r_pp, r_nn) = param.refract(f_nn)
      (f_ss ++ r_ss, f_pp :+ r_pp, r_nn)
    })
    (ss :+ lir.StmtFunc(id, pp), nn)
  }
}
case class IrStatementIf(expr: IrExpr, branchOne: IrBlock, branchTwo: Option[IrBlock])(implicit ref: AST) extends IrStatement {
  def check(g: Global): (Global, List[ErrMsg]) = {
    val (kind, inerrs) = expr.getType(g)
    (g, (if (!(kind == Some(VariableBoolean) || kind == Some(VariableCallout))) List(ErrMsg(ErrText.invalidwhilecond)) else List[ErrMsg]()) ++
    inerrs ++ branchOne.check(g.chScope(g.scope))._2 ++ (if (branchTwo isEmpty) List[ErrMsg]() else branchTwo.get.check(g.chScope(g.scope))._2))
  }
  def refract(n: NameNote) = {
      val (ss, exp, n1) = expr.refract(n)
      val (blockOne, p_n2) = branchOne.refract(n1)
      val n2 = n.fusion(p_n2)
      branchTwo match {
        case Some(branchTwo) => {
          val (blockTwo, p_n3) = branchTwo.refract(n2)
          val n3 = n2.fusion(p_n3)
          (ss :+ lir.StmtIf(exp, blockOne, Some(blockTwo)), n3)
        }
        case None => (ss :+ lir.StmtIf(exp, blockOne, None), n2)
      }
  }
}
case class IrStatementFor(id: String, start: IrExpr, stop: IrExpr, block: IrBlock)(implicit ref: AST) extends IrStatement {
  def check(g: Global): (Global, List[ErrMsg]) = {
    val (kindst, inerrsst) = start.getType(g)
    val (kindfn, inerrsfn) = stop.getType(g)
    (g, 
      (g.getId(id) match {
        case Some(Scalar(VariableInt)) => List[ErrMsg]()
        case None => List(ErrMsg(ErrText.undefined(id)))
        case _ => List(ErrMsg(ErrText.variablemismatch(id)))
      }) ++
      (if (!(kindst == Some(VariableInt) || kindst == Some(VariableCallout))) List(ErrMsg(ErrText.invalidforcond)) else List[ErrMsg]()) ++ inerrsst ++
      (if (!(kindfn == Some(VariableInt) || kindfn == Some(VariableCallout))) List(ErrMsg(ErrText.invalidforcond)) else List[ErrMsg]()) ++ inerrsfn ++
      block.check(g.toLoop)._2)
  }
  def refract(n: NameNote) = {
    //val (newName, n2) = n.addName(id, lir.VariableScalar(VariableInt))
    val newName = n.getName(id)
    val (cmp, n3) = n.addTemp(VariableBoolean)
    val (ss1, expStart, n4) = start.refract(n3)
    val (ss2, expStop, n5) = stop.refract(n4)
    val (bb, p_n6) = block.refract(n5)
    val n6 = n5.fusion(p_n6)
    (ss1 :+ lir.StmtAssign(newName, AssignEQ, expStart) :+ lir.StmtLoop(
      lir.Block(ss2 ++ List(
        lir.StmtBinop(cmp, lir.VarLit(newName), BinopLT, expStop)
      )),
      lir.VarLit(cmp),
      lir.Block(List(
        lir.StmtAssign(newName, AssignPlusEQ, lir.IntLit(1))
      )), bb), n6)
  }
}
case class IrStatementWhile(expr: IrExpr, block: IrBlock)(implicit ref: AST) extends IrStatement {
  def check(g: Global): (Global, List[ErrMsg]) = {
    val (kind, inerrs) = expr.getType(g)
    (g, (if (!(kind == Some(VariableBoolean) || kind == Some(VariableCallout))) List(ErrMsg(ErrText.invalidwhilecond)) else List[ErrMsg]()) ++
    inerrs ++ block.check(g.toLoop)._2)
  }
  def refract(n: NameNote) = {
    val (ss, exp, n1) = expr.refract(n)
    val (bb, p_n2) = block.refract(n1)
    val n2 = n1.fusion(p_n2)
    (List(lir.StmtLoop(lir.Block(ss), exp, lir.Block(List[lir.Statement]()), bb)), n2)
  }
}
case class IrStatementWhileLimit(expr: IrExpr, limit: Option[Long], block: IrBlock)(implicit ref: AST) extends IrStatement {
  def check(g: Global): (Global, List[ErrMsg]) = {
    val (kind, inerrs) = expr.getType(g)
    (g, (if (!(kind == Some(VariableBoolean) || kind == Some(VariableCallout))) List(ErrMsg(ErrText.invalidwhilecond)) else List[ErrMsg]()) ++
    inerrs ++ (if (limit.isEmpty || limit.get <= 0) List(ErrMsg(ErrText.invalidwhilelimit)) else List[ErrMsg]()) ++
    block.check(g.toLoop)._2)
  }
  def refract(n: NameNote) = {
    val (ss, exp, n1) = expr.refract(n)
    val (counter, n2) = n1.addTemp(VariableInt)
    val (counterCmp, n3) = n2.addTemp(VariableBoolean)
    val (cmp, n4) = n3.addTemp(VariableBoolean)
    val (bb, p_n5) = block.refract(n4)
    val n5 = n4.fusion(p_n5)
    (List(lir.StmtAssign(counter, AssignEQ, lir.IntLit(0))) :+ lir.StmtLoop(
      lir.Block(List(
        lir.StmtBinop(counterCmp, lir.VarLit(counter), BinopLT, lir.IntLit(limit.get)),
        lir.StmtBinopShortCircuit(cmp, lir.VarLit(counterCmp), BinopAND, lir.Block(ss), exp)
      )),
      lir.VarLit(cmp),
      lir.Block(List(
        lir.StmtAssign(counter, AssignPlusEQ, lir.IntLit(1))
      )), bb), n5)
  }
}
case class IrStatementReturn(expr: Option[IrExpr])(implicit ref: AST) extends IrStatement {
  def check(g: Global): (Global, List[ErrMsg]) = (g.scope, expr) match {
    case (ScopeGlobal(), _) => (g, List(ErrMsg(ErrText.returnnotinmethod)))
    case (_, None) => g.scope match {
      case ScopeLoop(None) => (g, List[ErrMsg]())
      case ScopeMethod(None) => (g, List[ErrMsg]())
      case _ => (g, List(ErrMsg(ErrText.returnnoneed)))
    }
    case (_, Some(rexpr)) => {
      val (kind, inerrs) = rexpr.getType(g)
      g.scope match {
        case ScopeLoop(s) => inerrs match {
          case Some(err) => (g, List(err))
          case None => if (kind == s || kind == VariableCallout) (g, List[ErrMsg]()) else (g, List(ErrMsg(ErrText.returnmismatch)))
        }
        case ScopeMethod(s) => inerrs match {
          case Some(err) => (g, List(err))
          case None => if (kind == s || kind == VariableCallout) (g, List[ErrMsg]()) else (g, List(ErrMsg(ErrText.returnmismatch)))
        }
        // Never reach here. Just to ignore warning messsage
        case ScopeGlobal() => (g, List[ErrMsg]())
      }
    }
  }
  def refract(n: NameNote) = {
    expr match {
      case Some(expr) => {
        val (ss, exp, n2) = expr.refract(n)
        (ss :+ lir.StmtReturn(Some(exp)), n2)
      }
      case None => {
        (List(lir.StmtReturn(None)), n)
      }
    }
  }
}
case class IrStatementBreak()(implicit ref: AST) extends IrStatement {
  def check(g: Global): (Global, List[ErrMsg]) = g.scope match {
    case ScopeLoop(_) => (g, List[ErrMsg]())
    case _ => (g, List(ErrMsg(ErrText.breaknotinloop)))
  }
  def refract(n: NameNote) = (List(lir.StmtBreak()), n)
}
case class IrStatementContinue()(implicit ref: AST) extends IrStatement {
  def check(g: Global): (Global, List[ErrMsg]) = g.scope match {
    case ScopeLoop(_) => (g, List[ErrMsg]())
    case _ => (g, List(ErrMsg(ErrText.continuenotinloop)))
  }
  def refract(n: NameNote) = (List(lir.StmtContinue()), n)
}

object IrStatement {
  def generate(implicit tree: AST): IrStatement = tree.getType() match {
    case DecafParserTokenTypes.EQUALS => IrStatementAssign(
      AssignEQ, IrLocation.generate(Helper.getnthChild(tree, 1)), IrExpr.generate(Helper.getnthChild(tree, 2)))
    case DecafParserTokenTypes.PLUSEQUALS => IrStatementAssign(
      AssignPlusEQ, IrLocation.generate(Helper.getnthChild(tree, 1)), IrExpr.generate(Helper.getnthChild(tree, 2)))
    case DecafParserTokenTypes.MINUSEQUALS => IrStatementAssign(
      AssignMinusEQ, IrLocation.generate(Helper.getnthChild(tree, 1)), IrExpr.generate(Helper.getnthChild(tree, 2)))
    case DecafParserTokenTypes.METHOD_CALL => IrStatementCall(
      Helper.getnthChild(tree, 1).getText(),
      if (tree.getNumberOfChildren <= 1) List[IrParam]() else Helper.parentToList(Helper.getnthChild(tree, 2), IrParam.generate(_)))
    case DecafParserTokenTypes.TK_if => IrStatementIf(
      IrExpr.generate(Helper.getnthChild(tree, 1)),
      IrBlock.generate(Helper.getnthChild(tree, 2)),
      Helper.getnthChild(tree, 3) match {
        case null => None
        case x => Some(IrBlock.generate(Helper.getnthChild(x, 1)))
      })
    case DecafParserTokenTypes.TK_for => IrStatementFor(
      Helper.getnthChild(tree, 1).getText(),
      IrExpr.generate(Helper.getnthChild(tree, 2)),
      IrExpr.generate(Helper.getnthChild(tree, 3)),
      IrBlock.generate(Helper.getnthChild(tree, 4)))
    case DecafParserTokenTypes.TK_while if tree.getNumberOfChildren() == 2 => IrStatementWhile(
      IrExpr.generate(Helper.getnthChild(tree, 1)),
      IrBlock.generate(Helper.getnthChild(tree, 2)))
    case DecafParserTokenTypes.TK_while => IrStatementWhileLimit(
      IrExpr.generate(Helper.getnthChild(tree, 1)),
      Helper.stringToLong(Helper.getnthChild(tree, 2).getText()),
      IrBlock.generate(Helper.getnthChild(tree, 3)))
    case DecafParserTokenTypes.TK_return => IrStatementReturn(
      Helper.getnthChild(tree, 1) match {
        case null => None
        case x => Some(IrExpr.generate(x))
      })
    case DecafParserTokenTypes.TK_break => IrStatementBreak()
    case DecafParserTokenTypes.TK_continue => IrStatementContinue()
    case DecafParserTokenTypes.FIELD_DECL => IrStatementFieldDecl(
      Helper.getnthChild(tree, 1).getType() match {
        case DecafParserTokenTypes.TK_int => VariableInt
        case DecafParserTokenTypes.TK_boolean => VariableBoolean
      },
      Helper.siblingsToList(Helper.getnthChild(tree, 2), IrVariable.generate(_))
    )
  }
}
