package ir

import antlr.collections.AST
import edu.mit.compilers.grammar.{ DecafParser, DecafParserTokenTypes, DecafScanner, DecafScannerTokenTypes }
import util.Helper

abstract class IrVariable(implicit val ref: AST) {
  def getId: String
}

case class IrVariableScalar(id: String)(implicit ref: AST) extends IrVariable {
  def getId = id
}
case class IrVariableVector(id: String, size: Option[Long])(implicit ref: AST) extends IrVariable {
  def getId = id
}

object IrVariable {
  def generate(implicit tree: AST): IrVariable = tree.getType() match {
    case DecafParserTokenTypes.VARIABLE_FIELD => IrVariableScalar(Helper.getnthChild(tree, 1).getText())
    case DecafParserTokenTypes.ARRAY_FIELD => IrVariableVector(
      Helper.getnthChild(tree, 1).getText(),
      Helper.stringToLong(Helper.getnthChild(tree, 2).getText())
    )
  }
}

