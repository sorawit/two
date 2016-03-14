package cfg

class Node(var from: Set[Node], var stmts: List[Statement],
  var next: Option[Node], var anext: Option[(lir.Param, Node)]) extends Ordered[Node] {

    /*
      if anext not empty:
        if lir.Param is true:
          go to anext
        else:
          go to next
      else:
        go to next
    */

    def addBranch(cond: lir.Param, des: Node) = {
      this.anext = Some((cond, des))
      des.from += this
    }
    def addLink(des: Node) = {
      this.next = Some(des)
      des.from += this
    }
    def dfs(inp: Set[Node]): Set[Node] = {
      val mid = next match {
        case None => inp
        case Some(n) => if (inp contains n) inp else n.dfs(inp + n)
      }
      anext match {
        case None => mid
        case Some((cond, an)) => if (mid contains an) mid else an.dfs(mid + an)
      }
    }

    def getDefs(g: Graph): Set[String] = (if (stmts.nonEmpty) stmts(0).getDefs() else Set[String]()) intersect g.locals.getAllScalar
    def getUsesInside(g: Graph): Set[String] = (if (stmts.nonEmpty) 
      stmts(0).getUses().flatMap({ f => f match {
        case lir.VarLit(s) => Set(s)
        case _ => Set[String]()
      }})
      else Set[String]()) intersect g.locals.getAllScalar
    def getUsesRightAfter(g: Graph): Set[String] = (if (anext.nonEmpty) 
      anext.get._1 match {
        case lir.VarLit(s) => Set(s)
        case _ => Set[String]()
      }
      else Set[String]()) intersect g.locals.getAllScalar

    def replaceDef(to: String) = {
      stmts = List(stmts(0).replaceDef(to))
    }
    def replaceUseInside(from: String, to: String) = {
      if (stmts.size > 0) {
        stmts = List(stmts(0).replaceUse(lir.VarLit(from), lir.VarLit(to)))
      }
    }
    def replaceUseRightAfter(from: String, to: String) = {
      if (anext.nonEmpty && anext.get._1 == lir.VarLit(from)) {
        anext = Some((lir.VarLit(to), anext.get._2))
      }
    }
    
    def compare(that: Node) = this.hashCode() - that.hashCode()
    override def hashCode(): Int = System.identityHashCode(this)
    override def toString(): String = "Node@" + System.identityHashCode(this)
    def format(): String =
      "Node: " + System.identityHashCode(this) + "\n" +
      "Statments: " + "\n" + (("" /: stmts) {_ + "\t" + _.toString + "\n"}) +
      "From: " + from.toString + "\n" +
      "Next: " + next.toString + "\n" +
      "Anext: " + anext.toString + "\n"

  def insertAfter(statements: List[Statement]): Unit = {
    val newNode = new Node(Set(this), statements, this.next, this.anext)
    this.next = Some(newNode)
    this.anext = None
    if (newNode.next.nonEmpty) {
      newNode.next.get.from -= this
      newNode.next.get.from += newNode
    }
    if (newNode.anext.nonEmpty) {
      newNode.anext.get._2.from -= this
      newNode.anext.get._2.from += newNode
    }
  }

  def replaceWith(statements: List[Statement]): Unit = {
    this.stmts = statements
  }

  def label(): String = ".sym" + this.hashCode()

  def toAssembly(vars: lir.Vars, alreadyPrinted: Set[Node]): (lir.StringMap, String, Set[Node]) = {
    val (strings, assemblyStatement) = ((lir.StringMap(), "") /: this.stmts) { case ((partialStrings, partialStatements), statement) =>
      val (newStrings, newStatement) = statement.toAssembly(vars)
      (partialStrings ++ newStrings, partialStatements + newStatement)
    }
    val selfPrinted = alreadyPrinted + this

    val (stringsToANext, assemblyToAnext) = if (anext.nonEmpty) {
      val (str, cond) = anext.get._1.repr(vars)
      (strings ++ str, assemblyStatement + codegen.Templates.conditionalJump(cond, this.anext.get._2.label()))
    } else {
      (strings, assemblyStatement)
    }

    val (stringsNext, assemblyNext, printedNext) =
      if (this.next.nonEmpty) {
        if (selfPrinted.contains(this.next.get)) {
          (stringsToANext, assemblyToAnext + codegen.Templates.jump(this.next.get), selfPrinted)
        } else {
          val (str, assem, printed) = this.next.get.toAssembly(vars, selfPrinted)
          (stringsToANext ++ str, assemblyToAnext + assem, printed)
        }
      } else {
        (stringsToANext, assemblyToAnext, selfPrinted)
      }

    val (stringsANext, assemblyANext, printedANext) = if (this.anext.nonEmpty) {
      if (printedNext.contains(this.anext.get._2)) {
        (stringsNext, assemblyNext, printedNext)
      } else {
        val (str, assem, printed) = this.anext.get._2.toAssembly(vars, printedNext)
        (stringsNext ++ str, assemblyNext + assem, printed)
      }
    } else {
      (stringsNext, assemblyNext, printedNext)
    }

    (stringsANext, codegen.Templates.node(this.label(), assemblyANext), printedANext)
  }

  def predecessors(): Set[Node] = this.from

  def successors(): Set[Node] = {
    val left = if (this.next.nonEmpty) Some(this.next.get) else None
    val right = if (this.anext.nonEmpty) Some(this.anext.get._2) else None
    Set(left, right).flatten
  }
}

object Node {
  def empty(): Node = new Node(Set[Node](), List[Statement](), None, None)
  def single(stmt: Statement): Node = new Node(Set[Node](), List(stmt), None, None)
  def linker(to: Option[Node]): Node = {
    val ret = new Node(Set[Node](), List[Statement](), to, None)
    if (!to.isEmpty) {
      to.get.from += ret
    }
    ret
  }
}
