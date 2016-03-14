package cfg

import lir.Vars

abstract class Statement {
  def toAssembly(vars: Vars): (lir.StringMap, String)
  def getDefs(): Set[String]
  def getUses(): Set[lir.Param]
  def replaceDef(to: String): Statement
  def replaceUse(from: lir.VarLit, to: lir.VarLit): Statement
}

case class StmtAssign(t: String, o: ir.AssignType, f: lir.Param) extends Statement {
  def toAssembly(vars: Vars) = lir.StmtAssign(t, o, f).toAssembly(vars)
  def getDefs() = Set(t)
  def getUses() = Set(f)
  def replaceDef(to: String) = StmtAssign(to, o, f)
  def replaceUse(from: lir.VarLit, to: lir.VarLit) = StmtAssign(t, o, to)
}
case class StmtAssignArray(t: String, idx: lir.Param, o: ir.AssignType, f: lir.Param, size: Long) extends Statement {
  def toAssembly(vars: Vars) = lir.StmtAssignArray(t, idx, o, f, size).toAssembly(vars)
  def getDefs() = Set[String]()
  def getUses() = Set(idx, f)
  def replaceDef(to: String) = StmtAssignArray(to, idx, o, f, size)
  def replaceUse(from: lir.VarLit, to: lir.VarLit) = StmtAssignArray(t, if (idx == from) to else idx, o, if (f == from) to else f, size)
}
case class StmtBinop(t: String, e1: lir.Param, o: ir.BinopType, e2: lir.Param) extends Statement {
  def toAssembly(vars: Vars) = lir.StmtBinop(t, e1, o, e2).toAssembly(vars)
  def getDefs() = Set(t)
  def getUses() = Set(e1, e2)
  def replaceDef(to: String) = StmtBinop(to, e1, o, e2)
  def replaceUse(from: lir.VarLit, to: lir.VarLit) = StmtBinop(t, if (e1 == from) to else e1, o, if (e2 == from) to else e2)
}
case class StmtNegative(t: String, f: lir.Param) extends Statement {
  def toAssembly(vars: Vars) = lir.StmtNegative(t, f).toAssembly(vars)
  def getDefs() = Set(t)
  def getUses() = Set(f)
  def replaceDef(to: String) = StmtNegative(to, f)
  def replaceUse(from: lir.VarLit, to: lir.VarLit) = StmtNegative(t, to)
}
case class StmtNegate(t: String, f: lir.Param) extends Statement {
  def toAssembly(vars: Vars) = lir.StmtNegate(t, f).toAssembly(vars)
  def getDefs() = Set(t)
  def getUses() = Set(f)
  def replaceDef(to: String) = StmtNegate(to, f)
  def replaceUse(from: lir.VarLit, to: lir.VarLit) = StmtNegate(t, to)
}
case class StmtTernary(t: String, cond: lir.Param, ft: lir.Param, ff: lir.Param) extends Statement {
  def toAssembly(vars: Vars) = lir.StmtTernary(t, cond, ft, ff).toAssembly(vars)
  def getDefs() = Set(t)
  def getUses() = Set(cond, ft, ff)
  def replaceDef(to: String) = StmtTernary(to, cond, ft, ff)
  def replaceUse(from: lir.VarLit, to: lir.VarLit) = StmtTernary(t, if (cond == from) to else cond,
    if (ft == from) to else ft, if (ff == from) to else ff)
}
case class StmtIndex(t: String, f: String, idx: lir.Param, size: Long) extends Statement {
  def toAssembly(vars: Vars) = lir.StmtIndex(t, f, idx, size).toAssembly(vars)
  def getDefs() = Set(t)
  def getUses() = Set(idx)
  def replaceDef(to: String) = StmtIndex(to, f, idx, size)
  def replaceUse(from: lir.VarLit, to: lir.VarLit) = StmtIndex(t, f, if (idx == from) to else idx, size)
}
case class StmtAssignFunc(t: String, f: String, p: List[lir.Param]) extends Statement {
  def toAssembly(vars: Vars) = lir.StmtAssignFunc(t, f, p).toAssembly(vars)
  def getDefs() = Set(t)
  def getUses() = p.toSet
  def replaceDef(to: String) = StmtAssignFunc(to, f, p)
  def replaceUse(from: lir.VarLit, to: lir.VarLit) = StmtAssignFunc(t, f, p.map({ x => if (x == from) to else x }))
}
case class StmtFunc(f: String, p: List[lir.Param]) extends Statement {
  def toAssembly(vars: Vars) = lir.StmtFunc(f, p).toAssembly(vars)
  def getDefs() = Set[String]()
  def getUses() = p.toSet
  def replaceDef(to: String) = this
  def replaceUse(from: lir.VarLit, to: lir.VarLit) = StmtFunc(f, p.map({ x => if (x == from) to else x }))
}
case class StmtReturn(t: Option[lir.Param]) extends Statement {
  def toAssembly(vars: Vars) = lir.StmtReturn(t).toAssembly(vars)
  def getDefs() = Set[String]()
  def getUses() = if (t.nonEmpty) Set(t.get) else Set[lir.Param]()
  def replaceDef(to: String) = this
  def replaceUse(from: lir.VarLit, to: lir.VarLit) = StmtReturn(if (t == Some(from)) Some(to) else t)
}
