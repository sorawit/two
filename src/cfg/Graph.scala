package cfg

import lir._

class Graph(var name: String, var params: List[String], var start: Node, var locals: lir.Local) {
  def getAllNodes(): Set[Node] = start.dfs(Set(start))

  def checkRep(): Boolean = getAllNodes.forall({ node => 
    node.from.forall({ f => f.next == Some(node) || (!f.anext.isEmpty && f.anext.get._2 == node) }) &&
      (node.next.isEmpty || node.next.get.from.contains(node)) &&
      (node.anext.isEmpty || node.anext.get._2.from.contains(node))
  })

  def changeParam(f: String, t: String) = {
    params = params.map({ x => if (x == f) t else x })
  }

  def refine() = {
    // Remove unreachable nodes
    val reachable = getAllNodes()
    reachable foreach { node =>
      node.from = node.from intersect reachable
    }
    
    // Remove empty nodes
    reachable.filter({ n => n.stmts.isEmpty && n.anext.isEmpty && n.next.nonEmpty && n != start }) foreach { node =>
      val nextNode = node.next.get
      node.from foreach { f =>
        if (f.next == Some(node)) {
          f.addLink(nextNode)
        }
        if (f.anext.nonEmpty && f.anext.get._2 == node) {
          val branch = f.anext.get._1
          f.addBranch(branch, node.next.getOrElse(Node.empty()))
        }
      }
      nextNode.from -= node
    }
  }

  def format: String = "*starting at: " + start + "\n\n" + ("" /: getAllNodes()) {case (str, f) => str + f.format + "\n"}

  def toAssembly(): (StringMap, String) = {
    val otherVars = locals.locals filterKeys { k => !params.contains(k) }

    // println("function: " + name)
    // Make Vars
    val paramOffsets = params.zipWithIndex.map { case (v, i) =>
      (v -> s"${-8*(i+1)}(%rbp)")
    }.toMap

    val (otherVarOffsets, stackNeeded) = ((Map[String, String](), paramOffsets.size + 1) /: otherVars) {
      case ((partialMap, counter), (name, decl)) => {
        // println(name)
        decl match {
          case VariableScalar(kind) => (partialMap + (name -> s"${-8*counter}(%rbp)"), counter + 1)
          case VariableVector(kind, size) => (partialMap + (name -> s"${-8*(counter + size.toInt-1)}(%rbp)"), counter + size.toInt)
        }
      }
    }
    // println(locals)
    val offsets = paramOffsets ++ otherVarOffsets
    // println(offsets)
    // println(params)
    val (strings, blockAssembly, _) = start.toAssembly(Vars(offsets, "", ""), Set[Node]())

    // (strings, codegen.Templates.functionDefinition(name, paramOffsets.size, stackNeeded - 1, blockAssembly))
    (strings, codegen.Templates.functionDefinition2(name, params, offsets, stackNeeded - 1, blockAssembly))
  }
}
