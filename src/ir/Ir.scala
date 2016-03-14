package ir

import antlr.collections.AST
import util.Helper

abstract class Ir(implicit ref: AST) {
}

case class IrProgram(callouts: List[IrCalloutDecl], fields: List[IrFieldDecl], methods: List[IrMethodDecl])(implicit ref: AST) extends Ir {
  def check(g: Global): (Global, List[ErrMsg]) =
    ((g, List[ErrMsg]()) /: (callouts ++ fields ++ methods)) {case ((oldg, olderrs), decl) => decl.check(oldg) match {
      case (newg, newerrs) => (newg, olderrs ++ newerrs)
    }}
  def refract(): lir.Program = {
    val globals = lir.Global(callouts.map({_.id}).toSet, fields.flatMap({_.refract}).toMap, methods.map({m => (m.id, m.params.size)}).toMap)
    val functions = methods.map({_.refract(globals)})
    lir.Program(globals, functions) 
  }

}

object IrProgram {
  def generate(implicit tree: AST) = IrProgram(
    Helper.parentToList(Helper.getnthChild(tree, 1), IrCalloutDecl.generate(_)), 
    Helper.parentToList(Helper.getnthChild(tree, 2), IrFieldDecl.generate(_)), 
    Helper.parentToList(Helper.getnthChild(tree, 3), IrMethodDecl.generate(_)))
}
