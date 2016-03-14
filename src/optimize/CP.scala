package optimize

case class Definition(statement: cfg.Statement) {
  def defines(v: String): Boolean = {
    v == (statement match {
      case cfg.StmtAssign(t, o, f) => t
      case cfg.StmtAssignArray(t, idx, o, f, size) => t
      case cfg.StmtBinop(t, e1, o, e2) => t
      case cfg.StmtNegative(t, f) => t
      case cfg.StmtNegate(t, f) => t
      case cfg.StmtTernary(t, cond, ft, ff) => t
      case cfg.StmtIndex(t, f, idx, size) => t
      case cfg.StmtAssignFunc(t, f, p) => t
      case cfg.StmtFunc(f, p) => None
      case cfg.StmtReturn(t) => None
    })
  }

  def value(): Option[lir.Param] = {
    statement match {
      case cfg.StmtAssign(t, ir.AssignEQ, f) => Some(f)
      case _ => None
    }
  }
}

object CP {

  def genSym_(prefix: String): () => String = {
    var count = -1
      () => {
      count += 1
      s"${prefix}${count}"
    }
  }

  def definedVariable(statement: cfg.Statement): Option[String] = {
    statement match {
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
  }

  def definesConstant(statement: cfg.Statement): Boolean = {
    statement match {
      case cfg.StmtAssign(t, o, f) => f match {
        case lir.IntLit(i) => true
        case lir.BoolLit(b) => true
        case _ => false
      }
      case _ => false
    }
  }

  def apply(graph: cfg.Graph, optcount: Int): (Boolean, String) = {
    val genSym = genSym_(s"cp${optcount}|")

    val allDefinitions = (Set[Definition]() /: graph.getAllNodes()) { case (partialDefinitions, node) =>
      partialDefinitions ++ node.stmts.filter(definesConstant).map(Definition).toSet
    }

    // Analysis

    val nodes = graph.getAllNodes()
    var printing = ""

    def generatedDefinition(statement: cfg.Statement): Option[Definition] = {
      if (definesConstant(statement)) Some(Definition(statement)) else None
    }

    def killedDefinitions(statement: cfg.Statement): Set[Definition] = {
      val variable = definedVariable(statement)
      if (variable.isEmpty) {
        Set()
      }
      else {
        allDefinitions filter { definition => definition.defines(variable.get) }
      }
    }

    def GEN(node: cfg.Node): Set[Definition] = node.stmts.map(generatedDefinition).flatten.toSet
    def KILL(node: cfg.Node): Set[Definition] = node.stmts.map(killedDefinitions).flatten.toSet

    val IN = scala.collection.mutable.Map[cfg.Node, Set[Definition]]()
    val OUT = scala.collection.mutable.Map[cfg.Node, Set[Definition]]()
    val changed = scala.collection.mutable.Set[cfg.Node]()

    nodes foreach { node =>
      OUT += (node -> Set())
    }

    val entry = graph.start
    IN(entry) = Set()
    OUT(entry) = GEN(entry)
    changed ++= nodes - entry

    while (changed.nonEmpty) {
      val n = changed.head
      changed -= n

      IN(n) = Set()
      n.predecessors() foreach { p =>
        IN(n) = IN(n) union OUT(p)
      }

      val old = OUT(n)
      OUT(n) = GEN(n) union (IN(n) -- KILL(n))

      if (OUT(n) != old) {
        n.successors() foreach { s =>
          changed += s
        }
      }
    }

    var somethingChanged = false

    nodes foreach { node =>
      val statements = node.stmts.map { statement =>
        val replacement = statement match {
          case cfg.StmtAssign(t, ir.AssignEQ, lir.VarLit(rhs)) => {
            val reachingDefinitions = IN(node)
            val relevantDefinitions = reachingDefinitions filter { definition => definition.defines(rhs) }

            val values = relevantDefinitions.map({ d => d.value() }).flatten
            val replaceable = values.size == 1

            if (replaceable) {
              val newrhs = values.head
              cfg.StmtAssign(t, ir.AssignEQ, newrhs)
            }
            else {
              statement
            }
          }
          case _ => statement
        }

        replacement
      }

      node.replaceWith(statements)
    }

    (somethingChanged, printing)
  }
}
