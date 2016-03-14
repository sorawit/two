package ir

import antlr.collections.AST
import edu.mit.compilers.grammar.{ DecafParser, DecafParserTokenTypes }
import util.{ Helper, ErrText }

abstract class IrDecl(implicit ref: AST) extends Ir {
  def check(g: Global): (Global, List[ErrMsg])
}

case class IrCalloutDecl(id: String)(implicit ref: AST) extends IrDecl {
  def check(g: Global): (Global, List[ErrMsg]) = g.getId(id) match {
    case Some(_) => (g, List(ErrMsg(ErrText.duplicatecallout(id))))
    case None => (g.addSymbol(id -> Callout()), List[ErrMsg]())
  }
}
object IrCalloutDecl {
  def generate(implicit tree: AST) = IrCalloutDecl(tree.getFirstChild().getText())
}

case class IrFieldDecl(kind: VariableType, ids: List[IrVariable])(implicit ref: AST) extends IrDecl {
  def check(g: Global): (Global, List[ErrMsg]) =
    ((g, List[ErrMsg]()) /: (ids)) {case ((oldg, olderrs), v) => v match {
      case IrVariableScalar(id) => oldg.getId(id) match {
        case Some(_) => (oldg, olderrs :+ ErrMsg(ErrText.duplicatefield(id)))
        case None => (oldg.addSymbol(id -> Scalar(kind)), olderrs)
      }
      case IrVariableVector(id, size) => (oldg.getId(id), size) match {
        case (_, Some(i)) if i <= 0 => (oldg, olderrs :+ ErrMsg(ErrText.badarraysize(id)))
        case (_, None) => (oldg, olderrs :+ ErrMsg(ErrText.intoverflow(Helper.getnthChild(v.ref, 2).getText())))
        case (Some(_), _) => (oldg, olderrs :+ ErrMsg(ErrText.duplicatefield(id)))
        case (None, Some(s)) => (oldg.addSymbol(id -> Vector(kind, s)), olderrs)
      }
    }}
  def refract(): Map[String, lir.VariableDecl] = (Map[String, lir.VariableDecl]() /: ids) {(m, v) => v match {
    case IrVariableScalar(id) => m + (id -> lir.VariableScalar(kind))
    case IrVariableVector(id, Some(s)) => m + (id -> lir.VariableVector(kind , s))
  }}
}
object IrFieldDecl {
  def generate(implicit tree: AST) = IrFieldDecl(
    Helper.getnthChild(tree, 1).getType() match {
      case DecafParserTokenTypes.TK_int => VariableInt
      case DecafParserTokenTypes.TK_boolean => VariableBoolean
    },
    Helper.siblingsToList(Helper.getnthChild(tree, 2), IrVariable.generate(_))
  )
}

case class IrMethodDecl(kind: Option[VariableType], id: String, params: List[(VariableType, String)], block: IrBlock)(implicit ref: AST) extends IrDecl {
  def check(g: Global): (Global, List[ErrMsg]) = g.getId(id) match {
    case Some(_) => (g, List(ErrMsg(ErrText.duplicatemethod(id))) ++ block.check(g.chScope(ScopeMethod(kind)).addLocal(params))._2)
    case None => if( params.size == params.map({ case (kind, id) => (id, kind) }).toMap.size )
      (g.addSymbol(id -> Method(kind, params.map(_._1))), block.check(g.chScope(ScopeMethod(kind)).addSymbol(id -> Method(kind, params.map(_._1))).addLocal(params))._2)
    else 
      (g, List(ErrMsg(ErrText.duplicatemethodparams(id))) ++ block.check(g.chScope(ScopeMethod(kind)).addSymbol(id -> Method(kind, params.map(_._1))).addLocal(params))._2)
  }
  def refract(g: lir.Global): lir.Function = {
    val initialNameNote = NameNote(g, lir.Local(), Map[String, String]())
    val paramsNameNote = (initialNameNote /: params) ({ case (nameNote, (vType, vName)) => nameNote.addName(vName, lir.VariableScalar(vType))._2 })
    val (blockBlock, blockNameNote) = block.refract(paramsNameNote)
    val finalBlock = kind match {
      case None => blockBlock.addReturn()
      case _ => blockBlock
    }
    return lir.Function(id, params.map({_._2}).map(blockNameNote.getName), finalBlock, blockNameNote.local)
  }
}
object IrMethodDecl {
  def generate(implicit tree: AST) = IrMethodDecl(
    Helper.getnthChild(tree, 1).getType() match {
      case DecafParserTokenTypes.TK_int => Some(VariableInt)
      case DecafParserTokenTypes.TK_boolean => Some(VariableBoolean)
      case DecafParserTokenTypes.TK_void => None
    },
    Helper.getnthChild(tree, 2).getText(),
    Helper.parentToList(Helper.getnthChild(tree, 3), x => (
      Helper.getnthChild(x, 1).getType() match {
        case DecafParserTokenTypes.TK_int => VariableInt
        case DecafParserTokenTypes.TK_boolean => VariableBoolean
      },
      Helper.getnthChild(x, 2).getText()
    )),
    IrBlock.generate(Helper.getnthChild(tree, 4))
  )
}

