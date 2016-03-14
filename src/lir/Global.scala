package lir

import codegen.Templates

/*
 * callout: string representing the callout name
 * variable: see the above class
 * method: string representing the name and an integer representing number of parameters
 */
case class Global(callouts: Set[String], variables: Map[String, VariableDecl], methods: Map[String, Int]) {
  def getSizeArray(s: String) = variables.get(s) match {
    case Some(VariableVector(_, s)) => s
    case _ => 0
  }
  def contains(s: String) = callouts.contains(s) || variables.contains(s) || methods.contains(s)
  def addMethods(s: String, p: Int) = Global(callouts, variables, methods + (s -> p))
  def format(i: Int) =
    Helper.ptabs(i) + "Callouts: " + callouts.mkString(", ") + "\n" +
    Helper.ptabs(i) + "Variables:\n" + variables.map({ case (name, kind) => Helper.ptabs(i+1) + name + ": " + kind + "\n" }).mkString

  def toAssembly(): String = {
    variables.map { case (name, decl) =>
      decl match {
        case VariableScalar(kind) => Templates.globalScalar(name)
        case VariableVector(kind, size) => Templates.globalVector(name, size)
      }
    }.mkString
  }
}
