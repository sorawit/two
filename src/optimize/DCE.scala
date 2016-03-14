package optimize

case class Variable(name: String) {}

object DCE {

  def extractParam(p: lir.Param): Option[String] = {
    p match {
      case lir.VarLit(v) => Some(v)
      case _ => None
    }
  }

  def variablesUsed(statement: cfg.Statement): Set[Variable] = {
    (statement match {
      case cfg.StmtAssign(t, o, f) => Set(f).map(extractParam)
      case cfg.StmtAssignArray(t, idx, o, f, size) => Set(idx, f).map(extractParam)
      case cfg.StmtBinop(t, e1, o, e2) => Set(e1, e2).map(extractParam)
      case cfg.StmtNegative(t, f) => Set(f).map(extractParam)
      case cfg.StmtNegate(t, f) => Set(f).map(extractParam)
      case cfg.StmtTernary(t, cond, ft, ff) => Set(cond, ft, ff).map(extractParam)
      case cfg.StmtIndex(t, f, idx, size) => Set(idx).map(extractParam)
      case cfg.StmtAssignFunc(t, f, p) => p.map(extractParam).toSet
      case cfg.StmtFunc(f, p) => p.map(extractParam).toSet
      case cfg.StmtReturn(Some(v)) => Set(v).map(extractParam)
      case _ => Set(None)
    }).flatten.map(Variable)
  }

  def lhs(statement: cfg.Statement): Option[Variable] = {
    val name = statement match {
      case cfg.StmtAssign(t, o, f) => Some(t)
      case cfg.StmtAssignArray(t, idx, o, f, size) => Some(t)
      case cfg.StmtBinop(t, e1, o, e2) => Some(t)
      case cfg.StmtNegative(t, f) => Some(t)
      case cfg.StmtNegate(t, f) => Some(t)
      case cfg.StmtTernary(t, cond, ft, ff) => Some(t)
      case cfg.StmtIndex(t, f, idx, size) => Some(t)
      case cfg.StmtAssignFunc(t, f, p) => Some(t)
      case cfg.StmtFunc(f, p) => None
      case cfg.StmtReturn(t) => None
    }

    if (name.nonEmpty) Some(Variable(name.get)) else None
  }

  def apply(graph: cfg.Graph, optcount: Int): (Boolean, String) = {
    var printing = ""

    val nodes = graph.getAllNodes()
    val exits = nodes filter { n =>
      (false /: n.stmts) { case (hasReturn, statement) =>
        val isReturn = statement match {
          case cfg.StmtReturn(t) => true
          case _ => false
        }

        hasReturn || isReturn
      }
    }

    def USE(node: cfg.Node): Set[Variable] = {
      val statementUses = node.stmts.map(variablesUsed).flatten.toSet
      val branchUses = if (node.anext.nonEmpty) {
        Set(extractParam(node.anext.get._1)).flatten.map(Variable)
      }
      else {
        Set()
      }

      statementUses ++ branchUses
    }
    def DEF(node: cfg.Node): Set[Variable] = node.stmts.map(lhs).flatten.toSet

    val IN = scala.collection.mutable.Map[cfg.Node, Set[Variable]]()
    val OUT = scala.collection.mutable.Map[cfg.Node, Set[Variable]]()
    val changed = scala.collection.mutable.Set[cfg.Node]()

    (nodes -- exits) foreach { n =>
      IN(n) = Set()
    }

    exits foreach { exit =>
      OUT(exit) = Set()
      IN(exit) = USE(exit)
    }

    changed ++= nodes -- exits

    while (changed.nonEmpty) {
      val n = changed.head
      changed -= n

      OUT(n) = Set()
      n.successors() foreach { s =>
        OUT(n) = OUT(n) union IN(s)
      }

      val old = IN(n)
      IN(n) = USE(n) union (OUT(n) -- DEF(n))

      if (IN(n) != old) {
        changed ++= n.predecessors()
      }
    }

    var somethingChanged = false

    nodes foreach { node =>
      val statements = node.stmts.map { statement =>
        val shouldKeep = statement match {
          case cfg.StmtAssign(t, o, f) => !graph.locals.contains(t)
          case cfg.StmtAssignArray(t, idx, o, f, size) => true
          case cfg.StmtAssignFunc(t, f, p) => true
          case cfg.StmtFunc(f, p) => true
          case cfg.StmtReturn(t) => true
          case _ => false
        }

        val variable = lhs(statement)

        val live = variable.isEmpty || OUT(node).contains(variable.get)

        if (live || shouldKeep) {
          Some(statement)
        } else {
          somethingChanged = true
          None
        }
      }

      node.replaceWith(statements.flatten)
    }

    (somethingChanged, printing)
  }
}
