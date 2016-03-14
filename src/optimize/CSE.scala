package optimize

sealed trait BinaryOperator
object Plus extends BinaryOperator
object Minus extends BinaryOperator
object Times extends BinaryOperator
object Div extends BinaryOperator
object Mod extends BinaryOperator
object GT extends BinaryOperator
object LT extends BinaryOperator
object GTE extends BinaryOperator
object LTE extends BinaryOperator
object EQ extends BinaryOperator
object NEQ extends BinaryOperator
object AND extends BinaryOperator
object OR extends BinaryOperator

sealed trait UnaryOperator
object Neg extends UnaryOperator
object NOT extends UnaryOperator


abstract class Expression {
  def uses(variable: String): Boolean
  def variables(): Set[String]
}

case class BinaryOperation(op: BinaryOperator, left: String, right: String) extends Expression {
  def uses(v: String) = v == left || v == right
  def variables() = Set(left, right)
}
case class UnaryOperation(op: UnaryOperator, arg: String) extends Expression {
  def uses(v: String) = v == arg
  def variables() = Set(arg)
}
case class FunctionCall(name: String, args: List[String]) extends Expression {
  def uses(v: String) = args.contains(v)
  def variables() = args.toSet
}
case class ArrayAccess(name: String, index: String) extends Expression {
  def uses(v: String) = v == index
  def variables() = Set(name, index)
}
case class TernaryOperation(cond: String, ifTrue: String, ifFalse: String) extends Expression {
  def uses(v: String) = v == cond || v == ifTrue || v == ifFalse
  def variables() = Set(cond, ifTrue, ifFalse)
}
case class Identifier(name: String) extends Expression {
  def uses(v: String) = v == name
  def variables() = Set(name)
}
case class Empty() extends Expression {
  def uses(v: String) = false
  def variables() = Set[String]()
}

object CSE {

  def genSym_(prefix: String): () => String = {
    var count = -1
      () => {
      count += 1
      s"${prefix}${count}"
    }
  }

  def convertBinaryOperator(op: ir.BinopType): BinaryOperator = {
    op match {
      case ir.BinopPlus => Plus
      case ir.BinopMinus => Minus
      case ir.BinopTimes => Times
      case ir.BinopDiv => Div
      case ir.BinopMod => Mod
      case ir.BinopGT => GT
      case ir.BinopLT => LT
      case ir.BinopGTE => GTE
      case ir.BinopLTE => LTE
      case ir.BinopEQ => EQ
      case ir.BinopNEQ => NEQ
      case ir.BinopAND => AND
      case ir.BinopOR => OR
    }
  }

  def convertParam(p: lir.Param): String = {
    p match {
      case lir.VarLit(v) => v
      case lir.IntLit(l) => l.toString
      case lir.BoolLit(b) => (if (b) "true" else "false")
      case lir.StringLit(s) => s
    }
  }

  def generateExpressions(statement: cfg.Statement): Expression = {
    statement match {
      case cfg.StmtAssign(t, o, f) => {
        val rhs = convertParam(f)
        o match {
          case ir.AssignEQ => Empty()
          case ir.AssignPlusEQ => BinaryOperation(Plus, t, rhs)
          case ir.AssignMinusEQ => BinaryOperation(Minus, t, rhs)
        }
      }
      case cfg.StmtAssignArray(t, idx, o, f, size) => {
        val rhs = convertParam(f)
        o match {
          case ir.AssignEQ => Empty()
          case ir.AssignPlusEQ => BinaryOperation(Plus, t, rhs)
          case ir.AssignMinusEQ => BinaryOperation(Minus, t, rhs)
        }
      }
      case cfg.StmtBinop(t, e1, o, e2) => {
        val op = convertBinaryOperator(o)
        val left = convertParam(e1)
        val right = convertParam(e2)
        BinaryOperation(op, left, right)
      }
      case cfg.StmtNegative(t, f) => {
        val arg = convertParam(f)
        UnaryOperation(Neg, arg)
      }
      case cfg.StmtNegate(t, f) => {
        val arg = convertParam(f)
        UnaryOperation(NOT, arg)
      }
      case cfg.StmtTernary(t, cond, ft, ff) => {
        val switch = convertParam(cond)
        val ifTrue = convertParam(ft)
        val ifFalse = convertParam(ff)
        TernaryOperation(switch, ifTrue, ifFalse)
      }
      case cfg.StmtIndex(t, f, idx, size) => {
        val index = convertParam(idx)
        ArrayAccess(f, index)
      }
      case cfg.StmtAssignFunc(t, f, p) => {
        val args = p.map(convertParam)
        FunctionCall(f, args)
      }
      case cfg.StmtFunc(f, p) => {
        val args = p.map(convertParam)
        FunctionCall(f, args)
      }
      case cfg.StmtReturn(t) => {
        Empty()
      }
    }
  }

  def killVariables(statement: cfg.Statement): Option[String] = {
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

  def apply(graph: cfg.Graph, optcount: Int): (Boolean, String) = {
    val genSym = genSym_(s"cse${optcount}|")

    // Generate temp variables

    val stored = scala.collection.mutable.Map[Expression, String]()
    graph.getAllNodes() foreach { n =>
      val statements = n.stmts.map { statement =>
        val lhs = killVariables(statement)
        val expr = generateExpressions(statement)
        val shouldReplace = expr match {
          case _: FunctionCall => false
          case _: Empty => false
          case _ => true
        }

        if (lhs.nonEmpty && shouldReplace && graph.locals.contains(lhs.get)) {
          // skip if not stored anywhere and don't re-use function calls

          if (!stored.contains(expr)) {
            val loc = genSym()
            stored += (expr -> loc)
            graph.locals = graph.locals.addSameType(loc, lhs.get)
          }

          Some(cfg.StmtAssign(stored(expr), ir.AssignEQ, lir.VarLit(lhs.get)))
        }
        else {
          None
        }
      }

      n.insertAfter(statements.flatten)
    }

    val nodes = graph.getAllNodes()
    var printing = ""

    // GEN = expressions generated by statement
    // KILL = expressions killed by statement
    // Calculating an expression on RHS is a GEN for that expression.
    // Assigning to a variable is a KILL for all expressions that use that variable.

    val allExpressions = (Set[Expression]() /: nodes) {
      case (pStatements, node) =>
        pStatements.union(node.stmts.map(generateExpressions).toSet)
    }

    def expressionsKilledBy(variable: Option[String]): Set[Expression] = {
      if (variable.isEmpty) {
        Set()
      }
      else {
        allExpressions.filter({ expr => expr.uses(variable.get) })
      }
    }

    def GEN(node: cfg.Node): Set[Expression] = node.stmts.map(generateExpressions).toSet
    def KILL(node: cfg.Node): Set[Expression] = node.stmts.map({ stmt => expressionsKilledBy(killVariables(stmt)) }).flatten.toSet

    val IN = scala.collection.mutable.Map[cfg.Node, Set[Expression]]()
    val OUT = scala.collection.mutable.Map[cfg.Node, Set[Expression]]()
    val changed = scala.collection.mutable.Set[cfg.Node]()

    nodes.foreach({ n =>
      OUT += (n -> allExpressions)
    })

    val entry = graph.start
    IN(entry) = Set()
    OUT(entry) = GEN(entry)
    changed ++= nodes - entry

    while (changed.nonEmpty) {
      val n = changed.head
      changed -= n

      IN(n) = allExpressions
      n.predecessors() foreach { p =>
        IN(n) = IN(n) intersect OUT(p)
      }

      val old = OUT(n)
      OUT(n) = GEN(n) union (IN(n) -- KILL(n))

      if (OUT(n) != old) {
        n.successors() foreach { s =>
          changed += s
        }
      }
    }

    printing += "RESULTS\n"
    IN foreach { case (node, expressions)  =>
      printing += node + "\n"
      printing += node.stmts + "\n"
      printing += expressions + "\n"
      printing += "\n"
    }

    def isLocal(variable: String): Boolean = graph.locals.contains(variable)
    def isAvailable(expr: Expression, node: cfg.Node): Boolean = IN(node).contains(expr) && expr.variables().forall(isLocal(_))

    var somethingChanged = false
    val actuallyUsed = scala.collection.mutable.Set[String]()

    graph.getAllNodes() foreach { node =>
      val statements = node.stmts.map { statement =>
        val lhs = killVariables(statement)
        val rhs = generateExpressions(statement)
        if (lhs.nonEmpty && stored.contains(rhs) && isAvailable(rhs, node)) {
          // if we're actually keeping expr somewhere, and expr is available here
          val newrhs = stored(rhs)
          somethingChanged = true

          actuallyUsed += newrhs

          cfg.StmtAssign(lhs.get, ir.AssignEQ, lir.VarLit(newrhs))
        }
        else {
          statement
        }
      }

      node.replaceWith(statements)
    }

    // Remove unused temporaries

    val unused = stored.values.toSet -- actuallyUsed

    graph.getAllNodes() foreach { node =>
      val statements = node.stmts.map { statement =>
        val lhs = killVariables(statement)
        if (lhs.nonEmpty && unused.contains(lhs.get)) {
          graph.locals = graph.locals.minus(lhs.get)
          None
        }
        else {
          Some(statement)
        }
      }

      node.replaceWith(statements.flatten)
    }

    (somethingChanged, printing)
  }
}
