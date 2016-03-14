package util

import antlr.collections.AST
import ir.Atom

object Helper {

  // Conversion
  def stringToLong(str: String): Option[Long] = try {
    Some(str toLong)
  } catch {
    case e: Exception => try {
      Some(java.lang.Long.parseLong(str.substring(2), 16))
    } catch {
      case f: Exception => try {
        Some(java.lang.Long.parseLong("-" + str.substring(3), 16))
      } catch {
        case f: Exception => None
      }
    }
  }

  def stringToChar(str: String): Char = str.charAt(1) match {
    case '\\' => str.charAt(2) match {
      case 't' => '\t'
      case 'n' => '\n'
      case '\'' => '\''
      case '\"' => '\"'
      case '\\' => '\\'
    }
    case x => x
  }

  // AST Traverse
  def getnthSibling(tree: AST, n: Int): AST = n match {
    case 0 => tree
    case n => getnthSibling(tree.getNextSibling(), n - 1)
  }

  def getnthChild(tree: AST, n: Int): AST =
    getnthSibling(tree.getFirstChild(), n - 1)

  def siblingsToList[T](tree: AST, fun: AST => T): List[T] = tree match {
    case null => Nil
    case _ => fun(tree) :: siblingsToList(tree.getNextSibling(), fun)
  }

  def parentToList[T](tree: AST, fun: AST => T): List[T] =
    siblingsToList(tree.getFirstChild(), fun)

  // Debugging Representation
  def printAST(tree: AST, tabs: Int): Unit = {
    for (a <- 1 to tabs) {
      print("\t")
    }

    val (line, col) = getLocation(tree)
    println(line + " " + tree.getText() + "  " + tree.getType())
    if (tree.getFirstChild() != null) {
      printAST(tree.getFirstChild(), tabs + 1)
    }
    if (tree.getNextSibling() != null) {
      printAST(tree.getNextSibling(), tabs)
    }
  }

  def getLocation(tree: AST): (Int, Int) = {
    var queue = List(tree)

    while (!queue.isEmpty) {
      val current = queue.head
      queue = queue.tail

      if (current.getLine != 0) {
        return (current.getLine, current.getColumn)
      }
      else {
        val numChildren = current.getNumberOfChildren

        if (numChildren > 0) {
          val firstChild = current.getFirstChild

          var child = firstChild
          var children = List(firstChild)
          for (i <- 2 to numChildren) {
            child = child.getNextSibling
            children = children ++ List(child)
          }

          queue = queue ++ children
        }
      }
    }

    return (0, 0)
  }
}
