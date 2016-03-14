package lir

import codegen.Templates

abstract class Statement {
  def format(i: Int): String
  def toAssembly(vars: Vars): (StringMap, String)
  def toCfg(breakPt: Option[cfg.Node], ctnPt: Option[cfg.Node]): (cfg.Node, Option[cfg.Node])
}

case class StmtAssign(t: String, o: ir.AssignType, f: Param) extends Statement {
  def format(i: Int) = Helper.ptabs(i) + t +  " " + o + " " + f + "\n"
  def toAssembly(vars: Vars) = {
    val (strings, rhs) = f.repr(vars)
    (strings, if (vars.contains(t)) {
      o match {
        case ir.AssignEQ => Templates.assignment(vars(t), rhs)
        case ir.AssignPlusEQ => Templates.binaryOperation("add", vars(t), rhs, vars(t))
        case ir.AssignMinusEQ => Templates.binaryOperation("sub", vars(t), rhs, vars(t))
      }
    }
    else {
      val loc = t + "(%rip)"
      o match {
        case ir.AssignEQ => Templates.assignment(loc, rhs)
        case ir.AssignPlusEQ => Templates.binaryOperation("add", loc, rhs, loc)
        case ir.AssignMinusEQ => Templates.binaryOperation("sub", loc, rhs, loc)
      }
    })
  }
  def toCfg(breakPt: Option[cfg.Node], ctnPt: Option[cfg.Node]) = o match {
    case ir.AssignEQ => (cfg.Node.single(cfg.StmtAssign(t, o, f)), None)
    case ir.AssignPlusEQ => (cfg.Node.single(cfg.StmtBinop(t, lir.VarLit(t), ir.BinopPlus, f)), None)
    case ir.AssignMinusEQ => (cfg.Node.single(cfg.StmtBinop(t, lir.VarLit(t), ir.BinopMinus, f)), None)
  }
    (cfg.Node.single(cfg.StmtAssign(t, o, f)), None)
}
case class StmtAssignArray(t: String, idx: Param, o: ir.AssignType, f: Param, size: Long) extends Statement {
  def format(i: Int) = Helper.ptabs(i) + t + "[" + idx + "] " + o + " " + f + "\n"
  def toAssembly(vars: Vars) = {
    // vars should contain zeroth element of t unless it's global
    val (strings, rhs) = f.repr(vars)
    val (moreStrings, offset) = idx.repr(vars)
    (strings ++ moreStrings, if (vars.contains(t)) {
      o match {
        case ir.AssignEQ => Templates.localArrayAssignment(vars(t), offset, rhs, size)
        case ir.AssignPlusEQ => Templates.localArrayOpAssign("add", vars(t), offset, rhs, size)
        case ir.AssignMinusEQ => Templates.localArrayOpAssign("sub", vars(t), offset, rhs, size)
      }
    }
    else {
      o match {
        case ir.AssignEQ => Templates.globalArrayAssignment(t, offset, rhs, size)
        case ir.AssignPlusEQ => Templates.globalArrayOpAssign("add", t, offset, rhs, size)
        case ir.AssignMinusEQ => Templates.globalArrayOpAssign("sub", t, offset, rhs, size)
      }
    })
  }
  def toCfg(breakPt: Option[cfg.Node], ctnPt: Option[cfg.Node]) =
    (cfg.Node.single(cfg.StmtAssignArray(t, idx, o, f, size)), None)
}
case class StmtBinop(t: String, e1: Param, o: ir.BinopType, e2: Param) extends Statement {
  def format(i: Int) = Helper.ptabs(i) + t + " = " + e1 + " " + o + " " + e2 + "\n"
  def toAssembly(vars: Vars) = {
    val (stringsL, left) = e1.repr(vars)
    val (stringsR, right) = e2.repr(vars)
    (stringsL ++ stringsR, o match {
      case ir.BinopPlus => Templates.binaryOperation("add", left, right, vars(t))
      case ir.BinopMinus => Templates.binaryOperation("sub", left, right, vars(t))
      case ir.BinopTimes => Templates.binaryOperation("imul", left, right, vars(t))
      case ir.BinopDiv => Templates.division(left, right, vars(t))
      case ir.BinopMod => Templates.modulo(left, right, vars(t))
      case ir.BinopEQ => Templates.equals(left, right, vars(t))
      case ir.BinopGT => Templates.gt(left, right, vars(t))
      case ir.BinopGTE => Templates.gte(left, right, vars(t))
      case ir.BinopLT => Templates.lt(left, right, vars(t))
      case ir.BinopLTE => Templates.lte(left, right, vars(t))
      case ir.BinopNEQ => Templates.neq(left, right, vars(t))
      case _ => null
    })
  }
  def toCfg(breakPt: Option[cfg.Node], ctnPt: Option[cfg.Node]) =
    (cfg.Node.single(cfg.StmtBinop(t, e1, o, e2)), None)
}
case class StmtBinopShortCircuit(t: String, e1: Param, o: ir.BinopType, preE2: Block, e2: Param) extends Statement {
  def format(i: Int) = Helper.ptabs(i) + t + " = " + e1 + " " + o + " " + e2 + "\n"
  def toAssembly(vars: Vars) = {
    // preE2 executed when e1 doesn't short-circuit
    //     check e1
    //     if e1
    //     then t = e1
    //     else
    //         preE2
    //         t = e2
    val (stringsL, left) = e1.repr(vars)
    val (stringsE, preE) = preE2.toAssembly(vars)
    val (stringsR, right) = e2.repr(vars)

    (stringsL ++ stringsE ++ stringsR, o match {
      case ir.BinopAND => Templates.and(left, right, preE, vars(t))
      case ir.BinopOR => Templates.or(left, right, preE, vars(t))
      case _ => null
    })
  }
  def toCfg(breakPt: Option[cfg.Node], ctnPt: Option[cfg.Node]) = {
    val ret = cfg.Node.empty()
    val term = cfg.Node.empty()
    val simple = cfg.Node.single(cfg.StmtAssign(t, ir.AssignEQ, e1))
    val (complicated, complicatedTerm) = preE2.toCfg(breakPt, ctnPt)
    val nextComplicated = cfg.Node.single(cfg.StmtAssign(t, ir.AssignEQ, e2))
    complicatedTerm.addLink(nextComplicated)
    simple.addLink(term)
    nextComplicated.addLink(term)
    o match {
      case ir.BinopAND => {
        ret.addLink(simple)
        ret.addBranch(e1, complicated)
      }
      case ir.BinopOR => {
        ret.addLink(complicated)
        ret.addBranch(e1, simple)
      }
      case _ => null
    }
    (ret, Some(term))
  }
}
case class StmtNegative(t: String, f: Param) extends Statement {
  def format(i: Int) = Helper.ptabs(i) + t + " = -" + f + "\n"
  def toAssembly(vars: Vars) = {
    val (strings, rhs) = f.repr(vars)
    (strings, Templates.unaryOperation("neg", rhs, vars(t)))
  }
  def toCfg(breakPt: Option[cfg.Node], ctnPt: Option[cfg.Node]) =
    (cfg.Node.single(cfg.StmtNegative(t, f)), None)
}
case class StmtNegate(t: String, f: Param) extends Statement {
  def format(i: Int) = Helper.ptabs(i) + t + " = not " + f + "\n"
  def toAssembly(vars: Vars) = {
    val (strings, rhs) = f.repr(vars)
    (strings, Templates.not(rhs, vars(t)))
  }
  def toCfg(breakPt: Option[cfg.Node], ctnPt: Option[cfg.Node]) =
    (cfg.Node.single(cfg.StmtNegate(t, f)), None)
}
case class StmtTernary(t: String, cond: Param, ft: Param, ff: Param) extends Statement {
  def format(i: Int) = Helper.ptabs(i) + t + " = " + cond + " ? " + ft + " : " + ff + "\n"
  def toAssembly(vars: Vars) = {
    val (s0, condition) = cond.repr(vars)
    val (s1, truth) = ft.repr(vars)
    val (s2, falsehood) = ff.repr(vars)
    (s0 ++ s1 ++ s2, Templates.ternary(condition, truth, falsehood, vars(t)))
  }
  def toCfg(breakPt: Option[cfg.Node], ctnPt: Option[cfg.Node]) =
    (cfg.Node.single(cfg.StmtTernary(t, cond, ft, ff)), None)
}
case class StmtIndex(t: String, f: String, idx: Param, size: Long) extends Statement {
  def format(i: Int) = Helper.ptabs(i) + t + " = " + f + "[" + idx + "]" + "\n"
  def toAssembly(vars: Vars) = {
    val (strings, index) = idx.repr(vars)
    (strings,
      if (vars.contains(f)) {
        Templates.localArrayAccess(vars(f), index, vars(t), size)
      }
      else {
        Templates.globalArrayAccess(f, index, vars(t), size)
      }
    )
  }
  def toCfg(breakPt: Option[cfg.Node], ctnPt: Option[cfg.Node]) =
    (cfg.Node.single(cfg.StmtIndex(t, f, idx, size)), None)
}
case class StmtAssignFunc(t: String, f: String, p: List[Param]) extends Statement {
  def format(i: Int) = Helper.ptabs(i) + t + " = " + f + "(" + p + ")" + "\n"
  def toAssembly(vars: Vars) = {
    val (strings, args, argsStack) = ((StringMap(), List[String](), List[String]()) /: p) { case ((partialStrings, partialArgs, partialStack), arg) =>
      val (nextStrings, nextArg) = arg.repr(vars)
      val nextStack = arg.reprStackPlease(vars)
      (partialStrings ++ nextStrings, partialArgs :+ nextArg, partialStack :+ nextStack)
    }
    (strings, Templates.functionCall(f, args, argsStack, vars(t)))
  }
  def toCfg(breakPt: Option[cfg.Node], ctnPt: Option[cfg.Node]) =
    (cfg.Node.single(cfg.StmtAssignFunc(t, f, p)), None)
}
case class StmtFunc(f: String, p: List[Param]) extends Statement {
  def format(i: Int) = Helper.ptabs(i) + f + "(" + p + ")" + "\n"
  def toAssembly(vars: Vars) = {
    val (strings, args, argsStack) = ((StringMap(), List[String](), List[String]()) /: p) { case ((partialStrings, partialArgs, partialStack), arg) =>
      val (nextStrings, nextArg) = arg.repr(vars)
      val nextStack = arg.reprStackPlease(vars)
      (partialStrings ++ nextStrings, partialArgs :+ nextArg, partialStack :+ nextStack)
    }
    // println(vars)
    (strings, Templates.functionCall(f, args, argsStack, "%rax"))
  }
  def toCfg(breakPt: Option[cfg.Node], ctnPt: Option[cfg.Node]) =
    (cfg.Node.single(cfg.StmtFunc(f, p)), None)
}
case class StmtIf(cond: Param, b1: Block, b2: Option[Block]) extends Statement {
  def format(i: Int) =
    Helper.ptabs(i) + "IF " + cond + "\n" +
    Helper.ptabs(i) + "THEN\n" + b1.format(i+1) +
    (if (b2 isEmpty) "" else Helper.ptabs(i) + "ELSE\n" + b2.get.format(i+1))

  def toAssembly(vars: Vars) = {
    val (s0, condition) = cond.repr(vars)
    val (s1, ifTrue) = b1.toAssembly(vars)
    val (s2, ifFalse) = if (b2 isEmpty) {
      (StringMap(), "nop")
    }
    else {
      b2.get.toAssembly(vars)
    }

    (s0 ++ s1 ++ s2, Templates.ifElse(condition, ifTrue, ifFalse))
  }
  def toCfg(breakPt: Option[cfg.Node], ctnPt: Option[cfg.Node]) = {
    val ret = cfg.Node.empty()
    val term = cfg.Node.empty()
    val (anextBegin, anextEnd) = b1.toCfg(breakPt, ctnPt)
    ret.addBranch(cond, anextBegin)
    anextEnd.addLink(term)
    if (!b2.isEmpty) {
      val (bnextBegin, bnextEnd) = b2.get.toCfg(breakPt, ctnPt)
      ret.addLink(bnextBegin)
      bnextEnd.addLink(term)
    } else {
      ret.addLink(term)
    }
    (ret, Some(term))
  }
}
case class StmtLoop(preCond: Block, cond: Param, update: Block, b: Block) extends Statement {
  def format(i: Int) =
    Helper.ptabs(i) + "LOOP\n" +
    Helper.ptabs(i+1) + "PRECONDITION\n" + preCond.format(i+2) +
    Helper.ptabs(i+1) + "CONDITION " + cond + "\n" +
    Helper.ptabs(i+1) + "BLOCK\n" + b.format(i+2) +
    Helper.ptabs(i+1) + "UPDATE\n" + update.format(i+2)

  def toAssembly(vars: Vars) = {
    val breakLocation = Templates.genSym()
    val continueLocation = Templates.genSym()

    val (s0, init) = preCond.toAssembly(vars)
    val (s1, condition) = cond.repr(vars)
    val (s2, up) = update.toAssembly(vars)

    val loopVars = vars.toLoopVars(breakLocation, continueLocation)
    val (s3, body) = b.toAssembly(loopVars)

    (s0 ++ s1 ++ s2 ++ s3, Templates.loop(init, condition, up, body, continueLocation, breakLocation))
  }

  def toCfg(breakPt: Option[cfg.Node], ctnPt: Option[cfg.Node]) = {
    val term = cfg.Node.empty()
    val checker = cfg.Node.empty()
    val (uCfgBegin, uCfgEnd) = update.toCfg(breakPt, ctnPt)
    val (pCfgBegin, pCfgEnd) = preCond.toCfg(breakPt, ctnPt)
    val (bCfgBegin, bCfgEnd) = b.toCfg(Some(term), Some(uCfgBegin))
    pCfgEnd.addLink(term)
    pCfgEnd.addBranch(cond, bCfgBegin)
    bCfgEnd.addLink(uCfgBegin)
    uCfgEnd.addLink(pCfgBegin)
    (cfg.Node.linker(Some(pCfgBegin)), Some(term))
  /*  precond
    / | \
   |  b  |
   |  |  |
    --u  |
        /
      */
  }
}
case class StmtContinue() extends Statement {
  def format(i: Int) = Helper.ptabs(i) + "CONTINUE\n"

  def toAssembly(vars: Vars) = {
    (StringMap(), Templates.continue(vars.contLoc))
  }

  def toCfg(breakPt: Option[cfg.Node], ctnPt: Option[cfg.Node]) = {
    (cfg.Node.linker(ctnPt), Some(cfg.Node.empty()))
  }
}
case class StmtBreak() extends Statement {
  def format(i: Int) = Helper.ptabs(i) + "BREAK\n"

  def toAssembly(vars: Vars) = {
    (StringMap(), Templates.break(vars.breakLoc))
  }

  def toCfg(breakPt: Option[cfg.Node], ctnPt: Option[cfg.Node]) =
    (cfg.Node.linker(breakPt), Some(cfg.Node.empty()))
}
case class StmtReturn(t: Option[Param]) extends Statement {
  def format(i: Int) = Helper.ptabs(i) + "RETURN " + t + "\n"

  def toAssembly(vars: Vars) = {
    if (t nonEmpty) {
      val (strings, value) = t.get.repr(vars)
      (strings, Templates.ret(value))
    }
    else {
      (StringMap(), Templates.ret("$0"))
    }
  }

  def toCfg(breakPt: Option[cfg.Node], ctnPt: Option[cfg.Node]) =
    (cfg.Node.single(cfg.StmtReturn(t)), Some(cfg.Node.empty()))
}
