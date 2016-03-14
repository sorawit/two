package lir

import codegen.Templates

abstract class VariableDecl(val t: ir.VariableType)
case class VariableScalar(override val t: ir.VariableType) extends VariableDecl(t)
case class VariableVector(override val t: ir.VariableType, s: Long) extends VariableDecl(t)

abstract class Param {
  def repr(vars: Vars): (StringMap, String)
  def reprFromStack(vars: Vars): (StringMap, String)
  def reprStackPlease(vars: Vars): String
}
case class VarLit(s: String) extends Param {
  def repr(vars: Vars) = {
    if (s.head == '%') (StringMap(), s) else reprFromStack(vars)
  }
  def reprFromStack(vars: Vars) = {
    (StringMap(), (if (vars.contains(s)) vars(s) else s"${s}(%rip)"))
  }
  def reprStackPlease(vars: Vars) = vars.applyStackOnlyPlease(s)
}
case class IntLit(s: Long) extends Param {
  def repr(vars: Vars) = {
    (StringMap(), "$" + s)
  }
  def reprFromStack(vars: Vars) = (StringMap(), "^_^")
  def reprStackPlease(vars: Vars) = "^_^"
}
case class BoolLit(s: Boolean) extends Param {
  def repr(vars: Vars) = {
    (StringMap(), "$" + (if (s) 1 else 0))
  }
  def reprFromStack(vars: Vars) = (StringMap(), "^_^")
  def reprStackPlease(vars: Vars) = "^_^"
}
case class StringLit(s: String) extends Param {
  def repr(vars: Vars) = {
    val sym = Templates.genSym()
    (StringMap(Map(sym -> s)), "$" + sym)
  }
  def reprFromStack(vars: Vars) = (StringMap(), "^_^")
  def reprStackPlease(vars: Vars) = "^_^"
}

case class Local(locals: Map[String, VariableDecl]) {
  def getSizeArray(s: String) = locals.get(s) match {
    case Some(VariableVector(_, s)) => s
    case _ => 0
  }
  def contains(s: String) = locals.contains(s)
  def add(s: String, t: VariableDecl) = Local(locals + (s -> t))
  def addSameType(s: String, o: String) = locals get o match {
    case Some(x) => add(s, VariableScalar(x.t))
    case _ => null
  }
  def minus(s: String) = Local(locals - s)
  def combine(l: Local) = l match {
    case Local(x) => Local(locals ++ x)
  }
  def format(i: Int) = Helper.ptabs(i) + "LOCALS: " + locals + "\n"
  def size = locals.size
  def removeScalar = Local(locals.filter({ case (x, y) => y match {
    case VariableScalar(_) => false
    case VariableVector(_, _) => true
  }}))
  def getAllScalar = locals.filter({ case (x, y) => y match {
    case VariableScalar(_) => true
    case VariableVector(_, _) => false
  }}).keySet
}

object Local {
  def apply(): Local = Local(Map[String, VariableDecl]())
}

case class Vars(vars: Map[String, String], breakLoc: String, contLoc: String) {
  def ++(other: Vars) = Vars(vars ++ other.vars, "", "")
  def apply(key: String) = if (key.head == '%') key else vars.getOrElse(key, s"${key}(%rip)")
  def applyStackOnlyPlease(key: String) = vars.getOrElse(key, s"${key}(%rip)")
  def contains(key: String) = vars.contains(key)
  def toLoopVars(brk: String, cont: String) = Vars(vars, brk, cont)
}

object Vars {
  def apply(): Vars = Vars(Map[String, String](), "", "")
}

case class StringMap(strings: Map[String, String]) {
  def ++(other: StringMap) = StringMap(strings ++ other.strings)
  def apply(key: String) = strings(key)
}

object StringMap {
  def apply(): StringMap = StringMap(Map[String, String]())
}
