package ir

import antlr.collections.AST

sealed abstract class Identified
case class Scalar(kind: VariableType) extends Identified
case class Vector(kind: VariableType, size: Long) extends Identified
case class Method(kind: Option[VariableType], params: List[VariableType]) extends Identified
case class Callout() extends Identified

sealed abstract class ScopeType
case class ScopeGlobal() extends ScopeType
case class ScopeMethod(ret: Option[VariableType]) extends ScopeType
case class ScopeLoop(ret: Option[VariableType]) extends ScopeType

case class ErrMsg(msg: String)(implicit val ref: AST)

case class Global(scope: ScopeType, symbolsOuter: Map[String, Identified], symbolsInner: Map[String, Identified]) {
  def getId(s: String): Option[Identified] = (symbolsOuter.get(s), symbolsInner.get(s)) match {
    case (_, Some(id)) => Some(id)
    case (Some(id), _) => Some(id)
    case (None, None) => None
  }
  def getIdInner(s: String) = symbolsInner.get(s)
  def addSymbol(s: (String, Identified)) = Global(scope, symbolsOuter, symbolsInner + s)
  def addLocal(params: List[(VariableType, String)]) = Global(scope, symbolsOuter, symbolsInner ++ params.map({ case (kind, id) => (id, Scalar(kind)) }))
  def chScope(s: ScopeType) = Global(s, symbolsOuter ++ symbolsInner, Map[String, Identified]())
  def toLoop() = scope match {
    case ScopeGlobal() => chScope(ScopeLoop(None))
    case ScopeMethod(s) => chScope(ScopeLoop(s))
    case ScopeLoop(s) => chScope(ScopeLoop(s))
  }
}
