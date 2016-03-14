package lir

import codegen.Templates

case class Program(globals: Global, functions: List[Function]) {
  def format: String =
    "Globals:\n" + globals.format(1) +
    "Functions:\n" + functions.map(_.format(1)).mkString

  def toAssembly(opts: List[Boolean], debug: Boolean): String = {
    val vars = globals.toAssembly()
    val (globalStrings, funs) = ((StringMap(), "") /: functions) { case ((mapName, partialFuns), function) =>
      // val (strings, fun) = function.toAssembly(opts)
      val (strings, fun) = function.toAssembly2(opts, debug)
      (mapName ++ strings, partialFuns + fun)
    }
    // strings = {symbol => content}
    val strings = globalStrings.strings.map { s => Templates.string(s._1, s._2) }.mkString

    Templates.macros() + strings + vars + funs
  }
}
