package codegen

object Templates {
  val argLocations: Stream[Option[String]] = Stream("%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9").map(Some(_)) #::: Stream.continually[Option[String]](None)
  val paramLocations: Stream[String] = Stream("%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9") #::: Stream.from(16, 8).map( offset => s"$offset(%rbp)")

  def genSym_(): () => String = {
    val prefix = ".gensym"
    var count = -1
    () => {
      count += 1
      s"${prefix}${count}"
    }
  }

  val genSym = genSym_()

  def macros(): String = {
    """
.macro pushall
    push %rbx
    push %rcx
    push %rdx
    push %rsi
    push %rdi
    push %r8
    push %r9
    push %r10
    push %r11
    push %r12
    push %r13
    push %r14
    push %r15
.endm

.macro popall
    pop %r15
    pop %r14
    pop %r13
    pop %r12
    pop %r11
    pop %r10
    pop %r9
    pop %r8
    pop %rdi
    pop %rsi
    pop %rdx
    pop %rcx
    pop %rbx
.endm
"""
  }

  def globalScalar(name: String): String = {
    s"""
    .comm ${name}, 8, 8
"""
  }

  def globalVector(name: String, size: Long): String = {
    s"""
    .comm ${name}, ${size * 8}, 64
"""
  }

  def string(name: String, content: String): String = {
    s"""
${name}:
    .string ${content}
"""
  }

  def exitWithStatus(status: Int): String = {
    s"""
    mov $$${status}, %rbx
    mov $$1, %rax
    int $$0x80
"""
  }

  def functionDefinition(name: String, numParams: Int, numVariables: Int, body: String): String = {
    val locals = Stream.from(-8, -8).map( offset => s"$offset(%rbp)" ) take numParams
    val unpackParams = locals.zip(paramLocations).map { case (to, from) =>
      s"""
    mov ${from}, %r11
    mov %r11, ${to}
"""
    }.mkString
    val zeroEverything = (1 to numVariables).map( i => {
      s"    movq $$0, ${-(i*8)}(%rbp)\n"
    }).mkString("")

    val exit = exitWithStatus(-2)
    s"""
    .globl ${name}
${name}:
    enter $$(${numVariables} * 8), $$0
${zeroEverything}
${unpackParams}
    ${body}

    ${exit}
"""
  }

  def isRegister(v: String): Boolean = v.head == '%'

  def functionDefinition2(name: String, params: List[String], offsets: Map[String, String], numVariables: Int, body: String) = {

    val localsStack = Stream.from(-8, -8).map( offset => s"$offset(%rbp)" ) take params.size
    val unpackParamsToStack = localsStack.zip(paramLocations).map { case (to, from) =>
      s"""
    mov ${from}, %r11
    mov %r11, ${to}
"""
    }.mkString

    val localsRegister = params map { p => if (isRegister(p)) p else offsets(p) }
    val loadParams = localsRegister.zip(localsStack).filter({ case (to, from) => isRegister(to) }).map({ case (to, from) =>
      s"""
    mov ${from}, ${to}
"""
    }).mkString("")

    val zeroStack = (1 to numVariables).map( i => {
      s"    movq $$0, ${-(i*8)}(%rbp)\n"
    }).mkString("")

    val zeroRegs = (optimize.RA.registers).map( reg => {
      s"    movq $$0, ${reg}\n"
    }).mkString("")

    val exit = exitWithStatus(-2)

    s"""
    .globl ${name}
${name}:
    enter $$(${numVariables} * 8), $$0
${zeroStack}
${unpackParamsToStack}
${zeroRegs}
${loadParams}
    ${body}

    ${exit}
"""
  }

  def assignment(lhs: String, rhs: String): String = {
    if (Set(lhs, rhs).exists(isRegister)) {
      s"""
    mov ${rhs}, ${lhs}
"""
    }
    else {
      s"""
    mov ${rhs}, %r11
    mov %r11, ${lhs}
"""
    }
  }

  def binaryOperation(op: String, larg: String, rarg: String, result: String): String = {
    if (isRegister(result) && larg == result) {
      s"""
    ${op} ${rarg}, ${larg}
"""
    }
    else if (isRegister(result) && rarg == result && op != "sub") {
      s"""
    ${op} ${larg}, ${rarg}
"""
    }
    else if (isRegister(larg) && op != "sub") {
      s"""
    mov ${rarg}, %r10
    ${op} ${larg}, %r10
    mov %r10, ${result}
"""
    }
    else if (isRegister(rarg) && op != "sub") {
      s"""
    mov ${larg}, %r10
    ${op} ${rarg}, %r10
    mov %r10, ${result}
"""
    }
    else {
      s"""
    mov ${larg}, %r10
    mov ${rarg}, %r11
    ${op} %r11, %r10
    mov %r10, ${result}
"""
    }
  }

  def division(larg: String, rarg: String, result: String): String = {
    s"""
    mov ${larg}, %r10
    mov ${rarg}, %r11
    push %rbx
    push %rdx
    mov %r10, %rax
    mov %r11, %rdx
    mov %rdx, %rbx
    cqto
    idivq %rbx
    pop %rdx
    pop %rbx
    mov %rax, ${result}
"""
  }

  def modulo(larg: String, rarg: String, result: String): String = {
    s"""
    mov ${larg}, %r10
    mov ${rarg}, %r11
    push %rbx
    push %rdx
    mov %r10, %rax
    mov %r11, %rdx
    mov %rdx, %rbx
    cqto
    idivq %rbx
    mov %rdx, %rax
    pop %rdx
    pop %rbx
    mov %rax, ${result}
"""
  }

  def and(larg: String, rarg: String, preR: String, result: String): String = {
    val falsehood = genSym()
    val out = genSym()

    s"""
    cmpq $$0, ${larg}
    je ${falsehood}
    ${preR}
""" +
    (if (Set(rarg, result).exists(isRegister)) {
      s"""
    mov ${rarg}, ${result}
"""
    }
    else {
      s"""
    mov ${rarg}, %r10
    mov %r10, ${result}
"""
    }) +
    s"""
    jmp ${out}
${falsehood}:
    mov $$0, ${result}
${out}:
"""
  }

  def or(larg: String, rarg: String, preR: String, result: String): String = {
    val truth = genSym()
    val out = genSym()

    s"""
    cmpq $$0, ${larg}
    jne ${truth}
    ${preR}
    mov ${rarg}, ${result}
    jmp ${out}
${truth}:
    mov $$1, ${result}
${out}:
"""
  }

  def comparison(jump: String, larg: String, rarg: String, result: String): String = {
    val truth = genSym()
    val out = genSym()
    var jmp = jump

    (if (isRegister(larg)) {
      s"""
    cmpq ${rarg}, ${larg}
"""
    }
    else if (isRegister(rarg)) {
      jmp = jmp match {
        case "jg"  => "jl"
        case "jge" => "jle"
        case "jl"  => "jg"
        case "jle" => "jge"
        case _ => jmp
      }
      s"""
    cmpq ${larg}, ${rarg}
"""
    }
    else {
      s"""
    mov ${larg}, %r10
    mov ${rarg}, %r11
    cmpq %r11, %r10
"""
    }) +
    s"""
    ${jmp} ${truth}
    movq $$0, ${result}
    jmp ${out}
${truth}:
    movq $$1, ${result}
${out}:
"""
  }

  def equals(larg: String, rarg: String, result: String): String = {
    comparison("je", larg, rarg, result)
  }

  def gt(larg: String, rarg: String, result: String): String = {
    comparison("jg", larg, rarg, result)
  }

  def gte(larg: String, rarg: String, result: String): String = {
    comparison("jge", larg, rarg, result)
  }

  def lt(larg: String, rarg: String, result: String): String = {
    comparison("jl", larg, rarg, result)
  }

  def lte(larg: String, rarg: String, result: String): String = {
    comparison("jle", larg, rarg, result)
  }

  def neq(larg: String, rarg: String, result: String): String = {
    comparison("jne", larg, rarg, result)
  }

  def unaryOperation(op: String, arg: String, result: String): String = {
    if (isRegister(arg) && arg == result) {
      s"""
    ${op} ${arg}
"""
    }
    else {
      s"""
    mov ${arg}, %r11
    ${op} %r11
    mov %r11, ${result}
"""
    }
  }

  def not(arg: String, result: String): String = {
    val alreadyfalse = genSym()
    val out = genSym()

    s"""
    cmpq $$0, ${arg}
    jne ${alreadyfalse}
    movq $$1, ${result}
    jmp ${out}
${alreadyfalse}:
    movq $$0, ${result}
${out}:
"""
  }

  def forLoop(init: String, cond: String, update: String, body: String): String = {
    val bodytag = genSym()
    val updatetag = genSym()
    val condtag = genSym()
    val outside = genSym()

    s"""
    ${init}
    jmp ${condtag}
${bodytag}:
    ${body}
${updatetag}:
    ${update}
${condtag}:
    ${cond}
    jne ${bodytag}
${outside}:
"""
  }

  def whileLoop(cond: String, limitCheck: String, body: String): String = {
    val bodyLimit = genSym()
    val condLoop = genSym()
    val outside = genSym()

    s"""
${bodyLimit}:
    ${body}
    ${limitCheck}
    jne ${condLoop}
    jmp ${outside}
${condLoop}:
    ${cond}
    je ${bodyLimit}
${outside}:
"""
  }

  def loop(preCond: String, cond: String, update: String, body: String, updatetag: String, out: String): String = {
    val condtag = genSym()

    s"""
${condtag}:
    ${preCond}
    cmpq $$0, ${cond}
    je ${out}
    ${body}
${updatetag}:
    ${update}
    jmp ${condtag}
${out}:
"""
  }

  def ifElse(cond: String, truebody: String, elsebody: String): String = {
    val elsetag = genSym()
    val outside = genSym()

    s"""
    cmpq $$0, ${cond}
    je ${elsetag}
    ${truebody}
    jmp ${outside}
${elsetag}:
    ${elsebody}
${outside}:
"""
  }

  def functionCall(name: String, args: List[String], argsStack: List[String], result: String): String = {
    // Move args that are register to argsStack
    // Push regs
    // Move argsStack
    // Pop regs
    val moveToStack = (argsStack.zip(args)).filter({ case (st, ns) => isRegister(ns) }).map { pair =>
      val (argStack, argRegis) = pair
        s"""    mov ${argRegis}, ${argStack}
"""
    }.mkString("")

    val moveArgs = (args.zip(argsStack)).zip(argLocations).reverse.map { pair =>
      val ((arg, argStack), reg) = pair
      val argUsed = if (isRegister(arg)) argStack else arg
      val next = if (reg.nonEmpty) {
        s"    mov ${argUsed}, ${reg.get}"
      }
      else {
        s"    pushq ${argUsed}"
      }
      next
    }.mkString("", "\n", "\n")

    s"""
${moveToStack}
    pushall
${moveArgs}
    mov $$0, %rax
    call ${name}
    addq $$${math.max(0, (args.length - 6) * 8)}, %rsp
    popall
    mov %rax, ${result}
"""
  }

  def continue(update: String): String = {
    s"""
    jmp ${update}
"""
  }

  def break(outside: String): String = {
    s"""
    jmp ${outside}
"""
  }

  def asm(arg: String): String = {
    s"${arg}\n"
  }

  def sequence(args: String*): String = {
    args.mkString("")
  }

  def checkBounds(index: String, size: Long): String = {
    val error = genSym()
    val ok = genSym()
    val exit = exitWithStatus(-1)

    s"""
    mov $$${size}, %r10
    mov ${index}, %r11
    cmpq %r10, %r11
    jge ${error}
    cmpq $$0, %r11
    jl ${error}
    jmp ${ok}
${error}:
    ${exit}
${ok}:
    nop
"""
    (if (isRegister(index)) {
      s"""
   cmpq $$${size}, ${index}
"""
    }
    else {
      s"""
    mov ${index}, %r11
    cmpq $$${size}, %r11
"""
    }) +
    s"""
    jge ${error}
""" +
    (if (isRegister(index)) {
      s"""
    cmpq $$0, ${index}
"""
    }
    else {
      s"""
    cmpq $$0, %r11
"""
    }) +
    s"""
    jl ${error}
    jmp ${ok}
${error}:
    ${exit}
${ok}:
"""
  }

  def globalArrayAssignment(name: String, index: String, rhs: String, size: Long): String = {
    val pointer = s"${name}(, %r11, 8)"
    checkBounds(index, size) + (
      if (Set(index, rhs).forall(isRegister)) {
        s"""
    mov ${rhs}, ${name}(, ${index}, 8)
"""
      }
      else if (isRegister(rhs)) {
        s"""
    mov ${index}, %r11
    mov ${rhs}, ${pointer}
"""
      }
      else if (isRegister(index)) {
        s"""
    mov ${rhs}, %r10
    mov %r10, ${name}(, ${index}, 8)
"""
      }
      else {
        s"""
    mov ${rhs}, %r10
    mov ${index}, %r11
    mov %r10, ${pointer}
"""
      })
  }

  def globalArrayAccess(name: String, index: String, lhs: String, size: Long): String = {
    // offset + (%r11 * 8)
    val pointer = s"${name}(, %r11, 8)"
    checkBounds(index, size) + (
      if (Set(index, lhs).forall(isRegister)) {
        s"""
    mov ${name}(, ${index}, 8), ${lhs}
"""
      }
      else if (isRegister(lhs)) {
        s"""
    mov ${index}, %r11
    mov ${pointer}, ${lhs}
"""
      }
      else if (isRegister(index)) {
        s"""
    mov ${name}(, ${index}, 8), %r10
    mov %r10, ${lhs}
"""
      }
      else {
    s"""
    mov ${index}, %r11
    mov ${pointer}, %r10
    mov %r10, ${lhs}
"""
      })
  }

  def globalArrayOpAssign(op: String, name: String, index: String, rhs: String, size: Long): String = {
    val pointer = s"${name}(, %r11, 8)"
    checkBounds(index, size) + (
      if (isRegister(index)) {
        s"""
    mov ${rhs}, %r10
    ${op} ${name}(, ${index}, 8), %r10
    mov %r10, ${name}(, ${index}, 8)
"""
      }
      else {
        s"""
    mov ${index}, %r11
    mov ${rhs}, %r10
    ${op} ${pointer}, %r10
    mov %r10, ${pointer}
"""
    })
  }

  def localArrayAssignment(head: String, index: String, rhs: String, size: Long): String = {
    // %rbp + offset + (%r11 * 8)
    val pointer = head.slice(0, head.length - 1) + ", %r11, 8)"
    checkBounds(index, size) + (
      if (Set(index, rhs).forall(isRegister)) {
        s"""
    mov ${rhs}, ${head.slice(0, head.length - 1)}, ${index}, 8)
"""
      }
      else if (isRegister(index)) {
        s"""
    mov ${rhs}, %r11
    mov %r11, ${head.slice(0, head.length - 1)}, ${index}, 8)
"""
      }
      else if (isRegister(rhs)) {
        s"""
    mov ${index}, %r11
    mov ${rhs}, ${pointer}
"""
      }
      else {
        s"""
    mov ${rhs}, %r10
    mov ${index}, %r11
    mov %r10, ${pointer}
"""
      })
  }

  def localArrayAccess(head: String, index: String, lhs: String, size: Long): String = {
    val pointer = head.slice(0, head.length - 1) + ", %r11, 8)"
    checkBounds(index, size) + (
      if (Set(index, lhs).forall(isRegister)) {
        s"""
    mov ${head.slice(0, head.length - 1)}, ${index}, 8), ${lhs}
"""
      }
      else if (isRegister(index)) {
        s"""
    mov ${head.slice(0, head.length - 1)}, ${index}, 8), %r10
    mov %r10, ${lhs}
"""
      }
      else if (isRegister(lhs)) {
        s"""
    mov ${index}, %r11
    mov ${pointer}, ${lhs}
"""
      }
      else {
      s"""
    mov ${index}, %r11
    mov ${pointer}, %r10
    mov %r10, ${lhs}
"""
    })
  }

  def localArrayOpAssign(op: String, head: String, index: String, rhs: String, size: Long): String = {
    val pointer = head.slice(0, head.length - 1) + ", %r11, 8)"
    checkBounds(index, size) + (
      if (isRegister(index)) {
        s"""
    mov ${rhs}, %r10
    ${op} ${head.slice(0, head.length - 1)}, %r11, 8), %r10
    mov %r10, ${head.slice(0, head.length - 1)}, %r11, 8)
"""
      }
      else {
        s"""
    mov ${index}, %r11
    mov ${rhs}, %r10
    ${op} ${pointer}, %r10
    mov %r10, ${pointer}
"""
      })
  }

  def ternary(cond: String, ifTrue: String, ifFalse: String, result: String): String = {
    val falsehood = genSym()
    val out = genSym()

    s"""
    cmpq $$0, ${cond}
    je ${falsehood}
""" +
    (if (Set(ifTrue, ifFalse, result).forall(isRegister))
      s"    movq ${ifTrue}, ${result}"
    else
      s"    movq ${ifTrue}, %r11"
    ) +
    s"""
    jmp ${out}
${falsehood}:
""" +
    (if (Set(ifTrue, ifFalse, result).forall(isRegister))
      s"    movq ${ifFalse}, ${result}"
    else
      s"    movq ${ifFalse}, %r11"
    ) +
    s"""
    mov ${ifFalse}, %r11
${out}:
""" +
    (if (Set(ifTrue, ifFalse, result).forall(isRegister))
      s""
    else
      s"    movq %r11, ${result}"
    )
  }

  def ret(value: String): String = {
    s"""
    mov ${value}, %rax
    leave
    ret
"""
  }

  def node(label: String, contents: String): String = {
    s"""
${label}:
${contents}
"""
  }

  def conditionalJump(cond: String, dest: String): String = {
    s"""
    cmpq $$0, ${cond}
    jne ${dest}
"""
  }

  def jump(thing: cfg.Node): String = {
    val dest = thing.label()

    s"""
    jmp ${dest}
"""
  }
}
