package ir

import antlr.collections.AST
import edu.mit.compilers.grammar.{ DecafParser, DecafParserTokenTypes }
import util.{ Helper, ErrText }

abstract class IrExpr(implicit ref: AST) {
  def getIntLiteral: Option[Long] = None
  def getType(implicit g: Global): (Option[VariableType], Option[ErrMsg])
  def refract(n: NameNote): (List[lir.Statement], lir.Param, NameNote)
}
case class IrExprLocation(loc: IrLocation)(implicit ref: AST) extends IrExpr {
  def getType(implicit g: Global) = loc.getType(g)
  def refract(n: NameNote) = loc.refract(n)
}
case class IrExprCall(id: String, params: List[IrParam])(implicit ref: AST) extends IrExpr {
  def getType(implicit g: Global) = g.getId(id) match {
    case Some(Callout()) => (Some(VariableCallout), None)
    case Some(Method(None, _)) => (None, Some(ErrMsg(ErrText.voidasexpr(id))))
    case Some(Method(Some(kind), xs)) => {
      val tps = params map (_.getType(g))
      val rps = tps.map(_._1).flatten
      val res = tps.map(_._2).flatten
      if (IrParam.compareParams(rps, xs)) (Some(kind), None) else (None, Some(ErrMsg(ErrText.methodparamsmissing(id))))
    }
    case None => (None, Some(ErrMsg(ErrText.undefinedmethod(id))))
    case _ => (None, Some(ErrMsg(ErrText.methodmismatch(id))))
  }
  def refract(n: NameNote) = {
    val (ss, pp, nn) = ((List[lir.Statement](), List[lir.Param](), n) /: params) ({ case ((f_ss, f_pp, f_nn), param) =>
      val (r_ss, r_pp, r_nn) = param.refract(f_nn)
      (f_ss ++ r_ss, f_pp :+ r_pp, r_nn)
    })
    // ASSUME IT IS AN INT!
    val (tName, nn2) = nn.addTemp(VariableInt)
    (ss :+ lir.StmtAssignFunc(tName, id, pp), lir.VarLit(tName), nn2)
  }
}
case class IrExprSize(id: String)(implicit ref: AST) extends IrExpr {
  // IMPORTANT: MUTABLE OBJECT
  var size: Long = 0
  def getType(implicit g: Global) = g.getId(id) match {
    case Some(Vector(_, s)) => {
      size = s
      (Some(VariableInt), None)
    }
    case Some(_) => (None, Some(ErrMsg(ErrText.sizemismatch(id))))
    case None => (None, Some(ErrMsg(ErrText.undefined(id))))
  }
  def refract(n: NameNote) = (List[lir.Statement](), lir.IntLit(size), n)
}
case class IrExprNegative(expr: IrExpr)(implicit ref: AST) extends IrExpr {
  def getType(implicit g: Global) = expr.getType match {
    case (Some(VariableInt), None) => (Some(VariableInt), None)
    case (Some(VariableCallout), None) => (Some(VariableInt), None)
    case (Some(_), None) => (None, Some(ErrMsg(ErrText.negativemismatch)))
    case (_, e) => (None, e)
  }
  def refract(n: NameNote) = {
    val (ss, exp, n2) = expr.refract(n)
    val (tName, n3) = n2.addTemp(VariableInt)
    (ss :+ lir.StmtNegative(tName, exp), lir.VarLit(tName), n3)
  }
}
case class IrExprNegate(expr: IrExpr)(implicit ref: AST) extends IrExpr {
  def getType(implicit g: Global) = expr.getType match {
    case (Some(VariableBoolean), None) => (Some(VariableBoolean), None)
    case (Some(VariableCallout), None) => (Some(VariableBoolean), None)
    case (_, None) => (None, Some(ErrMsg(ErrText.negatemismatch)))
    case (_, e) => (None, e)
  }
  def refract(n: NameNote) = {
    val (ss, exp, n2) = expr.refract(n)
    val (tName, n3) = n2.addTemp(VariableBoolean)
    (ss :+ lir.StmtNegate(tName, exp), lir.VarLit(tName), n3)
  }
}
case class IrExprBinopInt(binop: BinopIntType, leftExpr: IrExpr, rightExpr: IrExpr)(implicit ref: AST) extends IrExpr {
  def getType(implicit g: Global) = (leftExpr.getType, rightExpr.getType) match {
    case ((Some(VariableInt), None), (Some(VariableInt), None)) => (Some(VariableInt), None)
    case ((Some(VariableInt), None), (Some(VariableCallout), None)) => (Some(VariableInt), None)
    case ((Some(VariableCallout), None), (Some(VariableInt), None)) => (Some(VariableInt), None)
    case ((Some(VariableCallout), None), (Some(VariableCallout), None)) => (Some(VariableInt), None)
    case ((_, None), (_, None)) => (None, Some(ErrMsg(ErrText.binopintmismatch)))
    case ((_, Some(e)), (_, _)) => (None, Some(e))
    case ((_, _), (_, Some(e))) => (None, Some(e))
  }
  def refract(n: NameNote) = {
    val (ss1, exp1, n1) = leftExpr.refract(n)
    val (ss2, exp2, n2) = rightExpr.refract(n1)
    val (tName, n3) = n2.addTemp(VariableInt)
    (ss1 ++ ss2 :+ lir.StmtBinop(tName, exp1, binop, exp2), lir.VarLit(tName), n3)
  }
}
case class IrExprBinopBoolean(binop: BinopBooleanType, leftExpr: IrExpr, rightExpr: IrExpr)(implicit ref: AST) extends IrExpr {
  def getType(implicit g: Global) = (leftExpr.getType, rightExpr.getType) match {
    case ((Some(VariableInt), None), (Some(VariableInt), None)) => (Some(VariableBoolean), None)
    case ((Some(VariableInt), None), (Some(VariableCallout), None)) => (Some(VariableBoolean), None)
    case ((Some(VariableCallout), None), (Some(VariableInt), None)) => (Some(VariableBoolean), None)
    case ((Some(VariableCallout), None), (Some(VariableCallout), None)) => (Some(VariableBoolean), None)
    case ((_, None), (_, None)) => (None, Some(ErrMsg(ErrText.binopintmismatch)))
    case ((_, Some(e)), (_, _)) => (None, Some(e))
    case ((_, _), (_, Some(e))) => (None, Some(e))
  }
  def refract(n: NameNote) = {
    val (ss1, exp1, n1) = leftExpr.refract(n)
    val (ss2, exp2, n2) = rightExpr.refract(n1)
    val (tName, n3) = n2.addTemp(VariableBoolean)
    (ss1 ++ ss2 :+ lir.StmtBinop(tName, exp1, binop, exp2), lir.VarLit(tName), n3)
  }
}
case class IrExprBinopEq(binop: BinopBooleanType, leftExpr: IrExpr, rightExpr: IrExpr)(implicit ref: AST) extends IrExpr {
  def getType(implicit g: Global) = (leftExpr.getType, rightExpr.getType) match {
    case ((Some(x), None), (Some(y), None)) if (x == VariableCallout || y == VariableCallout || x == y) => (Some(VariableBoolean), None)
    case ((_, None), (_, None)) => (None, Some(ErrMsg(ErrText.binopeqmismatch)))
    case ((_, Some(e)), (_, _)) => (None, Some(e))
    case ((_, _), (_, Some(e))) => (None, Some(e))
  }
  def refract(n: NameNote) = {
    val (ss1, exp1, n1) = leftExpr.refract(n)
    val (ss2, exp2, n2) = rightExpr.refract(n1)
    val (tName, n3) = n2.addTemp(VariableBoolean)
    (ss1 ++ ss2 :+ lir.StmtBinop(tName, exp1, binop, exp2), lir.VarLit(tName), n3)
  }
}
case class IrExprBinopAndOr(binop: BinopBooleanType, leftExpr: IrExpr, rightExpr: IrExpr)(implicit ref: AST) extends IrExpr {
  def getType(implicit g: Global) = (leftExpr.getType, rightExpr.getType) match {
    case ((Some(VariableBoolean), None), (Some(VariableBoolean), None)) => (Some(VariableBoolean), None)
    case ((Some(VariableBoolean), None), (Some(VariableCallout), None)) => (Some(VariableBoolean), None)
    case ((Some(VariableCallout), None), (Some(VariableBoolean), None)) => (Some(VariableBoolean), None)
    case ((Some(VariableCallout), None), (Some(VariableCallout), None)) => (Some(VariableBoolean), None)
    case ((_, None), (_, None)) => (None, Some(ErrMsg(ErrText.binopboolmismatch)))
    case ((_, Some(e)), (_, _)) => (None, Some(e))
    case ((_, _), (_, Some(e))) => (None, Some(e))
  }
  def refract(n: NameNote) = {
    val (ss1, exp1, n1) = leftExpr.refract(n)
    val (ss2, exp2, n2) = rightExpr.refract(n1)
    val (tName, n3) = n2.addTemp(VariableBoolean)
    (ss1 :+ lir.StmtBinopShortCircuit(tName, exp1, binop, lir.Block(ss2),  exp2), lir.VarLit(tName), n3)
  }
}
case class IrExprTernary(cond: IrExpr, exprTrue: IrExpr, exprFalse: IrExpr)(implicit ref: AST) extends IrExpr {
  def getType(implicit g: Global) = cond.getType match {
    case (Some(x), None) if (x == VariableBoolean || x == VariableCallout)  => (exprTrue.getType, exprFalse.getType) match {
      case ((Some(a), None), (Some(b), None)) if (a == VariableCallout || b == VariableCallout || a == b) => (Some(a), None)
      case ((_, None), (_, None)) => (None, Some(ErrMsg(ErrText.ternaryexprmismatch)))
      case ((_, Some(e)), (_, _)) => (None, Some(e))
      case ((_, _), (_, Some(e))) => (None, Some(e))
    }
    case (_, None) => (None, Some(ErrMsg(ErrText.ternarycondmismatch)))
    case (_, e) => (None, e)
  }
  def refract(n: NameNote) = {
    val (ss1, exp1, n1) = cond.refract(n)
    val (ss2, exp2, n2) = exprTrue.refract(n1)
    val (ss3, exp3, n3) = exprFalse.refract(n2)
    // ASSUME IT IS AN INT!
    val (tName, n4) = n3.addTemp(VariableInt)
    (ss1 ++ ss2 :+ lir.StmtTernary(tName, exp1, exp2, exp3), lir.VarLit(tName), n4)
  }
}
case class IrInteger(value: Option[Long])(implicit ref: AST) extends IrExpr {
  override def getIntLiteral: Option[Long] =  value;
  def getType(implicit g: Global) = value match {
    case Some(i) => (Some(VariableInt), None)
    case None => (None, Some(ErrMsg(ErrText.intoverflow(ref.getText()))))
  }
  def refract(n: NameNote) = (List[lir.Statement](), lir.IntLit(value.get), n)
}
case class IrCharacter(value: Char)(implicit ref: AST) extends IrExpr {
  def getType(implicit g: Global) = (None, None)
  def refract(n: NameNote) = null
}
case class IrBoolean(value: Boolean)(implicit ref: AST) extends IrExpr {
  def getType(implicit g: Global) = (Some(VariableBoolean), None)
  def refract(n: NameNote) = (List[lir.Statement](), lir.BoolLit(value), n)
}

object IrExpr {
  def generate(implicit tree: AST): IrExpr = tree.getType() match {
    case DecafParserTokenTypes.IDENTIFIER => IrExprLocation(IrLocation.generate(tree))
    case DecafParserTokenTypes.METHOD_CALL => IrExprCall(
      Helper.getnthChild(tree, 1).getText(),
      if (tree.getNumberOfChildren <= 1) List[IrParam]() else Helper.parentToList(Helper.getnthChild(tree, 2), IrParam.generate(_)))
    case DecafParserTokenTypes.AT => IrExprSize(Helper.getnthChild(tree, 1).getText())
    case DecafParserTokenTypes.MINUS if (tree.getNumberOfChildren == 1) => Helper.getnthChild(tree, 1).getType() match {
      case DecafParserTokenTypes.INTLITERAL => IrInteger(Helper.stringToLong("-" + Helper.getnthChild(tree, 1).getText()))
      case _ => IrExpr.generate(Helper.getnthChild(tree, 1)) match {
        case IrInteger(None) => IrInteger(None)
        case IrInteger(Some(value)) => IrInteger(Some(-value))
        case expr => IrExprNegative(expr)
      }
    }
    case DecafParserTokenTypes.EXCL => IrExprNegate(IrExpr.generate(Helper.getnthChild(tree, 1)))
    case DecafParserTokenTypes.PLUS => IrExprBinopInt(BinopPlus,
      IrExpr.generate(Helper.getnthChild(tree, 1)), IrExpr.generate(Helper.getnthChild(tree, 2)))
    case DecafParserTokenTypes.MINUS => IrExprBinopInt(BinopMinus,
      IrExpr.generate(Helper.getnthChild(tree, 1)), IrExpr.generate(Helper.getnthChild(tree, 2)))
    case DecafParserTokenTypes.TIMES => IrExprBinopInt(BinopTimes,
      IrExpr.generate(Helper.getnthChild(tree, 1)), IrExpr.generate(Helper.getnthChild(tree, 2)))
    case DecafParserTokenTypes.DIV => IrExprBinopInt(BinopDiv,
      IrExpr.generate(Helper.getnthChild(tree, 1)), IrExpr.generate(Helper.getnthChild(tree, 2)))
    case DecafParserTokenTypes.MOD => IrExprBinopInt(BinopMod,
      IrExpr.generate(Helper.getnthChild(tree, 1)), IrExpr.generate(Helper.getnthChild(tree, 2)))
    case DecafParserTokenTypes.EQ => IrExprBinopEq(BinopEQ,
      IrExpr.generate(Helper.getnthChild(tree, 1)), IrExpr.generate(Helper.getnthChild(tree, 2)))
    case DecafParserTokenTypes.NEQ => IrExprBinopEq(BinopNEQ,
      IrExpr.generate(Helper.getnthChild(tree, 1)), IrExpr.generate(Helper.getnthChild(tree, 2)))
    case DecafParserTokenTypes.GT => IrExprBinopBoolean(BinopGT,
      IrExpr.generate(Helper.getnthChild(tree, 1)), IrExpr.generate(Helper.getnthChild(tree, 2)))
    case DecafParserTokenTypes.LT => IrExprBinopBoolean(BinopLT,
      IrExpr.generate(Helper.getnthChild(tree, 1)), IrExpr.generate(Helper.getnthChild(tree, 2)))
    case DecafParserTokenTypes.GTE => IrExprBinopBoolean(BinopGTE,
      IrExpr.generate(Helper.getnthChild(tree, 1)), IrExpr.generate(Helper.getnthChild(tree, 2)))
    case DecafParserTokenTypes.LTE => IrExprBinopBoolean(BinopLTE,
      IrExpr.generate(Helper.getnthChild(tree, 1)), IrExpr.generate(Helper.getnthChild(tree, 2)))
    case DecafParserTokenTypes.AND => IrExprBinopAndOr(BinopAND,
      IrExpr.generate(Helper.getnthChild(tree, 1)), IrExpr.generate(Helper.getnthChild(tree, 2)))
    case DecafParserTokenTypes.OR => IrExprBinopAndOr(BinopOR,
      IrExpr.generate(Helper.getnthChild(tree, 1)), IrExpr.generate(Helper.getnthChild(tree, 2)))
    case DecafParserTokenTypes.QUES => IrExprTernary(IrExpr.generate(Helper.getnthChild(tree, 1)),
      IrExpr.generate(Helper.getnthChild(tree, 2)), IrExpr.generate(Helper.getnthChild(tree, 3)))
    case DecafParserTokenTypes.INTLITERAL => IrInteger(Helper.stringToLong(tree.getText()))
    case DecafParserTokenTypes.CHARLITERAL => IrCharacter(Helper.stringToChar(tree.getText()))
    case DecafParserTokenTypes.TK_true => IrBoolean(true)
    case DecafParserTokenTypes.TK_false => IrBoolean(false)
  }
}
