package ir

import antlr.collections.AST
import util.Helper

case class IrBlock(statements: List[IrStatement])(implicit ref: AST) extends Ir {
  def check(g: Global): (Global, List[ErrMsg]) = 
    ((g, List[ErrMsg]()) /: statements) {case ((oldg, olderrs), statement) => statement.check(oldg) match {
      case (newg, newerrs) => (newg, olderrs ++ newerrs)
    }}
  def refract(n: NameNote): (lir.Block, NameNote) = {
    val (stmts, nn) = ((List[lir.Statement](), n) /: statements) ({ case ((stms, ns), stmt) =>
      val (r_stms, r_ns) = stmt.refract(ns)
      (stms ++ r_stms, r_ns)
    })
    (lir.Block(stmts), nn)
  }
}
object IrBlock {
  def generate(implicit tree: AST) = IrBlock(Helper.parentToList(tree, IrStatement.generate(_)))
}
