package optimize

/*
Register allocation optimization: rename all local variables to 
  rax, rbx, rcx, rdx, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15, THEN
  t1, t2, t3, t4, ...
*/

abstract class Def(val f: String) {
  def replaceDef(t: String): Unit
}
case class TopDef(override val f: String, g: cfg.Graph) extends Def(f) {
  def replaceDef(t: String) = g.changeParam(f, t)
}
case class NodeDef(override val f: String, n: cfg.Node) extends Def(f) {
  def replaceDef(t: String) = n.replaceDef(t)
}

abstract class Use(val f: String) {
  def replaceUse(t: String): Unit
}
case class InsideUse(override val f: String, n: cfg.Node) extends Use(f) {
  def replaceUse(t: String) = n.replaceUseInside(f, t)
}
case class RightAfterUse(override val f: String, n: cfg.Node) extends Use(f) {
  def replaceUse(t: String) = n.replaceUseRightAfter(f, t)
}
case class ParamUse(override val f: String) extends Use(f) {
  def replaceUse(t: String) = { }
}

case class Web(v: String, var defs: Set[Def], var uses: Set[Use]) {
  def overlaps(w: Web) = (v == w.v) && (((defs intersect w.defs).nonEmpty) || (uses intersect w.uses).nonEmpty)
  def conflicts(w: Web) = this.conflictsHelper(w) || w.conflictsHelper(this)
  def conflictsHelper(w: Web) = uses.exists({ use => use match {
    case InsideUse(f, n) => w.defs.exists({ deff => RA.TOP(n).contains(deff.f) && (RA.IN(n) contains deff) })
    case RightAfterUse(f, n) => w.defs.exists({ deff => RA.MID(n).contains(deff.f) && (RA.OUT(n) contains deff) })
    case ParamUse(_) => w.uses contains(ParamUse(w.v))
  }})

  def combines(w: Web) = {
    defs = defs union w.defs
    uses = uses union w.uses
  }
  def addDefs(d: Def) = { defs += d }
  def addUses(u: Use) = { uses += u }
}

object RA {
  /*
    1. Get all node-pair reaching Defs
      - Problem: Def at the beginning of function?
    2. Combine pairs into web (set of pair)
    3. Find edges between each pair of webs
    4. Assign colors!
    5. Rename variables for each of the colored node
  */
  /*val registers = Set("%rax", "%rbx", "%rcx", "%rdx", "%rsi", "%rdi", "%r8", "%r9",
    "%r10", "%r11", "%r12", "%r13", "%r14", "%r15")*/
  val registers = Set("%rbx", "%rcx", "%rdx", "%rsi", "%rdi", "%r8", "%r9", "%r12", "%r13", "%r14", "%r15")
  // val registers = Set("%r12", "%r13", "%r14", "%r15")
  //val registers = Set[String]()

  val IN = scala.collection.mutable.Map[cfg.Node, Set[Def]]()
  val OUT = scala.collection.mutable.Map[cfg.Node, Set[Def]]()

  val TOP = scala.collection.mutable.Map[cfg.Node, Set[String]]()
  val MID = scala.collection.mutable.Map[cfg.Node, Set[String]]()
  val BOT = scala.collection.mutable.Map[cfg.Node, Set[String]]()

  def USES_INSIDE(node: cfg.Node, graph: cfg.Graph): Set[Use] = node.getUsesInside(graph).map({ s => InsideUse(s, node) })
  def USES_AFTER(node: cfg.Node, graph: cfg.Graph): Set[Use] = node.getUsesRightAfter(graph).map({ s => RightAfterUse(s, node) })
  def DEFS(node: cfg.Node, graph: cfg.Graph): Set[Def] = node.getDefs(graph).map({ s => NodeDef(s, node) })

  def reachingDef(graph: cfg.Graph) {
    val nodes = graph.getAllNodes()
    def GEN(node: cfg.Node): Set[Def] = IN(node).filter({ d => !node.getDefs(graph).contains(d.f) }) ++ DEFS(node, graph)

    var entry = graph.start
    val changed = scala.collection.mutable.Set[cfg.Node]()
    changed ++= nodes - entry
    for (node <- nodes) {
      OUT(node) = Set[Def]()
      IN(node) = Set[Def]()
    }

    IN(entry) = Set[Def]()
    for (eachVariable <- graph.locals.getAllScalar) {
      IN(entry) += TopDef(eachVariable, graph)
    }

    OUT(entry) = GEN(entry)

    while (changed.nonEmpty) {
      val n = changed.head
      changed -= n
      IN(n) = Set[Def]()
      for (f <- n.from) {
        IN(n) = IN(n) union OUT(f)
      }
      val old = OUT(n)
      OUT(n) = GEN(n)
      if (OUT(n) != old) {
        n.successors() foreach { s =>
          changed += s
        }
      }
    }
  }

  def livenessAnalyze(graph: cfg.Graph) {
    val nodes = graph.getAllNodes()
    val exits = nodes.filter({ n => n.next == None && n.anext == None })

    def GEN_MID(node: cfg.Node): Set[String] = BOT(node) union node.getUsesRightAfter(graph)
    def GEN_TOP(node: cfg.Node): Set[String] = (MID(node) -- node.getDefs(graph)) union node.getUsesInside(graph)

    val changed = scala.collection.mutable.Set[cfg.Node]()
    changed ++= (nodes -- exits)

    for (node <- nodes) {
      TOP(node) = Set[String]()
    }
    for (exit <- exits) {
      BOT(exit) = Set[String]()
      MID(exit) = GEN_MID(exit)
      TOP(exit) = GEN_TOP(exit)
    }
    while (changed.nonEmpty) {
      val n = changed.head
      changed -= n
      BOT(n) = Set[String]()
      for (s <- n.successors) {
        BOT(n) = BOT(n) union TOP(s)
      }
      val old = TOP(n)
      MID(n) = GEN_MID(n)
      TOP(n) = GEN_TOP(n)
      if (TOP(n) != old) {
        changed ++= n.from
      }
    }
  }

  def apply(graph: cfg.Graph): String = {

    val nodes = graph.getAllNodes()
    var allWebs = Set[Web]()

    reachingDef(graph)
    livenessAnalyze(graph)

    /*
    println(graph.format)
    for (n <- nodes) {
      println(n)
      println("USEINSIDE" + n.getUsesInside(graph))
      println("USERIGHTAFTER" + n.getUsesRightAfter(graph))
      println("DEF" + n.getDefs(graph))
      println("IN\t" + IN(n))
      println("OUT\t" + OUT(n))
      println("TOP\t" + TOP(n))
      println("MID\t" + MID(n))
      println("BOT\t" + BOT(n))
    }
    */

    for (eachVariable <- graph.locals.getAllScalar) {
      allWebs += Web(eachVariable, Set(TopDef(eachVariable, graph)), Set[Use]())
    }
    for (eachParam <- graph.params) {
      allWebs += Web(eachParam, Set(TopDef(eachParam, graph)), Set(ParamUse(eachParam)))
    }

    for (node <- nodes) {
      for (d <- node.getDefs(graph)) {
        allWebs += Web(d, Set(NodeDef(d, node)), Set(RightAfterUse(d, node)))
      }
      val in = IN(node)
      val out = OUT(node)
      for (use <- USES_INSIDE(node, graph)) {
        val newWeb = Web(use.f, Set[Def](), Set(use))
        for (d <- in if d.f == use.f) {
          newWeb.addDefs(d)
        }
        allWebs += newWeb
      }
      for (use <- USES_AFTER(node, graph)) {
        val newWeb = Web(use.f, Set[Def](), Set(use))
        for (d <- out if d.f == use.f) {
          newWeb.addDefs(d)
        }
        allWebs += newWeb
      }
    }

    var fixPoint = false
    while (!fixPoint) {
      fixPoint = true
      for (web1 <- allWebs if fixPoint) {
        for (web2 <- allWebs if fixPoint; if web1 != web2; if web1.overlaps(web2)) {
          fixPoint = false
          web1.combines(web2)
          allWebs -= web2
        }
      }
    }

    val edges = scala.collection.mutable.Map[Web, Int]()
    val webIntersectCache = scala.collection.mutable.Map[(Web, Web), Boolean]()
    val stack = scala.collection.mutable.MutableList[Web]()

    for (w1 <- allWebs) {
      edges(w1) = 0
      for (w2 <- allWebs; if w1 != w2) {
        if (w1 conflicts w2) {
          edges(w1) = edges(w1) + 1
          webIntersectCache((w1, w2)) = true
        } else {
          webIntersectCache((w1, w2)) = false
        }
      }
    }

    while (edges.nonEmpty) {
      val (web, neighbor) = edges.toSet.minBy({ x: (Web, Int) => x._2 })
      if (neighbor < registers.size) {
        stack += web
      }
      edges -= web
      for (d <- edges.keySet) {
        if (webIntersectCache((d, web))) {
          edges(d) = edges(d) - 1
        }
      }
    }

    val colors = scala.collection.mutable.Map[Web, String]()

    for (web <- stack.reverse) {
      var remainColors = registers
      for (d <- colors.keySet if webIntersectCache((web, d))) {
        remainColors = remainColors - colors(d)
      }
      colors(web) = remainColors.head
    }

    for (web <- allWebs if !colors.contains(web)) {
      var usedColors = Set[String]()
      for (d <- colors.keySet if webIntersectCache((web, d))) {
        usedColors = usedColors + colors(d)
      }
      if ((registers -- usedColors).nonEmpty) {
        colors(web) = (registers -- usedColors).head
      } else {
        var i = 0
        while (!colors.contains(web)) {
          val vname = "?v" + i
          if (!colors.keySet.exists({ d => webIntersectCache((web, d)) && colors(d) == vname })) {
            colors(web) = vname
          }
          i = i + 1
        }
      }
    }

    val activeColors = colors.values.toSet
    // println(activeColors.size)

    for (web <- allWebs) {
      val color = colors(web)
      for (deff <- web.defs) {
        // println(deff.f + " to " + color)
        deff.replaceDef(color)
      }
      for (use <- web.uses) {
        // println(use.f + " to " + color)
        use.replaceUse(color)
      }
    }

    graph.locals = graph.locals.removeScalar
    for (color <- activeColors) {
      graph.locals = graph.locals.add(color, lir.VariableScalar(ir.VariableInt))
    }
    return ""
  }
}
