package ir

case class NameNote(global: lir.Global, local: lir.Local, rename: Map[String, String]) {
  def getSizeArray(s: String) = 
    if (global.contains(s)) global.getSizeArray(s)
    else local.getSizeArray(rename(s))
  def getName(s: String): String = rename getOrElse(s, s)
  def addName(s: String, k: lir.VariableDecl): (String, NameNote) = addNameHelper(s, 1, k)
  def addTemp(k: VariableType): (String, NameNote) = addNameHelper("~temp", 1, lir.VariableScalar(k))
  def addTempSameType(o: String): (String, NameNote) = addNameHelper("~temp", 1, lir.VariableScalar(local.locals.get(o).getOrElse(lir.VariableScalar(VariableInt)).t))
  def addNameHelper(s: String, i: Int, k: lir.VariableDecl): (String, NameNote) = {
    val newName = s + "~" + i
    if (global.contains(newName) || local.contains(newName)) addNameHelper(s, i + 1, k) 
    else (newName, NameNote(global, local.add(newName, k), rename + (s -> newName)))
  }
  def fusion(n: NameNote) = NameNote(global, local combine n.local, rename)
}
