package ir

import antlr.collections.AST
import util.{ Helper, ErrText }

abstract class IrLocation(val id: String)(implicit ref: AST) {
  def getType(implicit g: Global): (Option[VariableType], Option[ErrMsg])
  def refract(n: NameNote): (List[lir.Statement], lir.Param, NameNote)
}

case class IrLocationScalar(override val id: String)(implicit ref: AST) extends IrLocation(id) {
  def getType(implicit g: Global): (Option[VariableType], Option[ErrMsg]) = g.getId(id) match {
    case Some(Scalar(kind)) => (Some(kind), None)
    case None => (None, Some(ErrMsg(ErrText.undefined(id))))
    case _ => (None, Some(ErrMsg(ErrText.scalarmismatch(id))))
  }
  def refract(n: NameNote) = (List[lir.Statement](), lir.VarLit(n getName id), n)
}
case class IrLocationVector(override val id: String, index: IrExpr)(implicit ref: AST) extends IrLocation(id) {
  def getType(implicit g: Global): (Option[VariableType], Option[ErrMsg]) = g.getId(id) match {
    case Some(Vector(kind, _)) => index.getType match {
      case (_, Some(e)) => (None, Some(e))
      case (Some(VariableCallout), None) => (Some(kind), None)
      case (Some(VariableInt), None) => index.getIntLiteral match {
        case Some(i) if i < 0 => (None, Some(ErrMsg(ErrText.indexnegative(id))))
        case _ => (Some(kind), None)
      }
      case (_, None) => (None, Some(ErrMsg(ErrText.indexmismatch(id))))
    }
    case None => (None, Some(ErrMsg(ErrText.undefined(id))))
    case _ => (None, Some(ErrMsg(ErrText.arraymismatch(id))))
  }
  def refract(n: NameNote) = {
    val chId = n.getName(id)
    val (ss, idx, n2) = index.refract(n)
    val (tName, n3) = n2.addTempSameType(chId) 
    (ss :+ lir.StmtIndex(tName, chId, idx, n3.getSizeArray(id)), lir.VarLit(tName), n3)
  }
}

object IrLocation {
  def generate(implicit tree: AST): IrLocation = tree.getNumberOfChildren match {
    case 0 => IrLocationScalar(tree.getText())
    case 1 => IrLocationVector(tree.getText(), 
      IrExpr.generate(Helper.getnthChild(tree, 1)))
  }
}
