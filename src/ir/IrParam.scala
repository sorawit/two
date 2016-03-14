package ir

import antlr.collections.AST
import edu.mit.compilers.grammar.{ DecafParser, DecafParserTokenTypes }
import util.Helper

abstract class IrParam(implicit ref: AST) extends Ir {
  def getType(implicit g: Global): (Option[VariableType], Option[ErrMsg])
  def refract(n: NameNote): (List[lir.Statement], lir.Param, NameNote)
}

case class IrParamString(value: String)(implicit ref: AST) extends IrParam {
  def getType(implicit g: Global): (Option[VariableType], Option[ErrMsg]) = (None, None)
  def refract(n: NameNote) = (List[lir.Statement](), lir.StringLit(value), n)
}
case class IrParamExpr(value: IrExpr)(implicit ref: AST) extends IrParam {
  def getType(implicit g: Global): (Option[VariableType], Option[ErrMsg]) = value.getType
  def refract(n: NameNote) = value.refract(n)
}

object IrParam {
  def generate(implicit tree: AST): IrParam = tree.getType() match {
    case DecafParserTokenTypes.STRINGLITERAL => IrParamString(tree.getText())
    case _ => IrParamExpr(IrExpr.generate(tree))
  }

  def compareParams(xx: List[VariableType], yy: List[VariableType]): Boolean = 
    if (xx == Nil) 
      if (yy == Nil) true
      else false
    else
      if (yy == Nil) false
      else {
        val x = xx.head
        val xs = xx.tail
        val y = yy.head
        val ys = yy.tail
        (x == VariableCallout || y == VariableCallout || x == y) && compareParams(xs, ys)
      }
}
