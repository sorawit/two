package lir

import codegen.Templates

case class Function(name: String, params: List[String], block: Block, locals: Local) {
  def format(i: Int) =
    Helper.ptabs(i) + "Function " + name + ":\n" +
    Helper.ptabs(i+1) + "Params: " + params + "\n" +
    block.format(i+1) + locals.format(i+1)

  def toAssembly(opts: List[Boolean]): (StringMap, String) = {
    val otherVars = locals.locals filterKeys { k => !params.contains(k) }

    // Make Vars
    val paramOffsets = params.zipWithIndex.map { case (v, i) =>
      (v -> s"${-8*(i+1)}(%rbp)")
    }.toMap

    val (otherVarOffsets, stackNeeded) = ((Map[String, String](), paramOffsets.size + 1) /: otherVars) {
      case ((partialMap, counter), (name, decl)) =>
        decl match {
          case VariableScalar(kind) => (partialMap + (name -> s"${-8*counter}(%rbp)"), counter + 1)
          case VariableVector(kind, size) => (partialMap + (name -> s"${-8*(counter + size.toInt-1)}(%rbp)"), counter + size.toInt)
        }
    }
    val offsets = paramOffsets ++ otherVarOffsets
    val (strings, blockAssembly) =
      if (opts.size > 0 && opts(0))
        block.toAssembly(Vars(offsets, "", ""))
      else
        block.toAssembly(Vars(offsets, "", ""))

    // (strings, Templates.functionDefinition(name, paramOffsets.size, stackNeeded - 1, blockAssembly))
    (strings, Templates.functionDefinition2(name, params, offsets, stackNeeded - 1, blockAssembly))
  }

  def toAssembly2(opts: List[Boolean], debug: Boolean): (StringMap, String) = {
    val cfg = toCfg()

    var optCount = -1
    var needToOptimize = opts.exists(x => x)
    // needToOptimize = false

    val (doCSE, doCP, doDCE, doRA) = (opts(0), opts(1), opts(2), opts(3))

    // call all optimizations until cfg fixpoint
    while (needToOptimize) {
      needToOptimize = false

      if (doCSE) {
        optCount += 1
        val (somethingChanged, debugging) = optimize.CSE(cfg, optCount)
        if (debug) println(debugging)
        needToOptimize ||= somethingChanged
      }

      if (doCP) {
        optCount += 1
        val (somethingChanged, debugging) = optimize.CP(cfg, optCount)
        if (debug) println(debugging)
        needToOptimize ||= somethingChanged
      }

      if (doDCE) {
        optCount += 1
        val (somethingChanged, debugging) = optimize.DCE(cfg, optCount)
        if (debug) println(debugging)
        needToOptimize ||= somethingChanged
      }

      cfg.refine()
    }


    if (doRA) {
      cfg.refine
      // println(cfg.format)
      val debugging = optimize.RA(cfg)
      // println(cfg.format)
      if (debug) println(debugging)
    }
    cfg.refine
    // println(cfg.format)

    // codegen this function and return
    cfg.toAssembly()
  }

  def toCfg(): cfg.Graph = {
    val graph = new cfg.Graph(name, params, block.toCfg(None, None)._1, locals)
    graph.refine()
    graph
  }
}
