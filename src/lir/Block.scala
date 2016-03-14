package lir

case class Block(statements: List[Statement]) {
  def format(i: Int) = statements.map({_.format(i)}).mkString
  def addReturn() = Block(statements :+ StmtReturn(None))
  def toAssembly(vars: Vars): (StringMap, String) = {
    val (strings, assembly) = ((StringMap(), "") /: statements) { case ((partialStrings, partialStatements), statement) =>
      val (newStrings, newStatement) = statement.toAssembly(vars)
      (partialStrings ++ newStrings, partialStatements + newStatement)
    }
    (strings, assembly)
  }
  def toCfg(breakPt: Option[cfg.Node], ctnPt: Option[cfg.Node]): (cfg.Node, cfg.Node) = {
    var firstNode = cfg.Node.empty()
    var currentNode = firstNode
    var stop = false
    for (stmt <- statements) {
      val (appNode, newNode) = stmt.toCfg(breakPt, ctnPt)
      currentNode.addLink(appNode);
      currentNode = appNode;
      if (!currentNode.next.isEmpty && newNode.isEmpty) {
        stop = true
      }
      if (!stop && !newNode.isEmpty) {
        currentNode = newNode.get
      }
    }
    (firstNode, currentNode);
  }
}
