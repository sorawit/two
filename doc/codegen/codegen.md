Code Generation
===============


Division of Work
----------------

Since there were two phases we each took one.  Sorawit converted the high-level IR into a low-level
IR (IR -> LIR), and Jeremy converted the low-level IR into assembly code (LIR -> ASM).


Problem Statement Changes
-------------------------

The following are things we specify in our code generation that are not explicitly stated in the
problem statement.

* Any method (except `void` methods) whose execution "falls off" without returning a value triggers
  an `exit` (a runtime error) with return status -2 (or 254).  To circumvent this behavior for
  `void` methods, they should always return 0.  This allows a program's `main` method to return a
  status code of zero if it happens to be `void`.

* All fields are initialized to zeroes on declaration.  Scalar `int` and `boolean` fields are set to
  0 and false, respectively.  Arrays have each element initialized to the "zero" value for their
  type.

* `while` loops with a limit test the iteration count before the condition.  For example, if the
  limit is 1, the condition and loop body are only executed once each.

Design
------

### IR -> LIR

Each subclass of IR implements a method called `refract`, which essentially takes a global map of
variable renaming book object (we need to rename shadowing variables), and returns an LIR
representing its representation plus the updated version of the renaming object. Most of the work is
done using the visitor pattern with some functional language features such as list mapping and
folding.

The refract method also, as it seems fit, generates temporary variables in order to evaluate certain
expressions. For example, when it refactoring a statement `x = (a + b) + c`, it firstly generates a
temp variable call `~tempX` where X is an increasing integer. Then it adds a statement `~tempX = a +b`,
and later adds `x = ~tempX + c`. Most of the refactor methods share the same signature, except
that of the `IrExpr` subclasses because it additionally returns a variable name where the caller of
this `refract` function can access the evaluated expression.

By recursively calling the `refract` method on the topmost `IrProgram`, the program returns the LIR
representation of the whole program, as well as the Set / Map of all global callouts and variables,
as well as the local variables of each function.

### LIR -> ASM

Each subclass of LIR implements a method called `toAssembly`, which takes in a `Vars` object and
returns a tuple of the `StringMap` generated for that node and the `String` text of the assembly
code generated.  A `StringMap` associates the label for a string with its content.  A `Vars` object
is a wrapped `Map[String, String]` that keeps track of the location of each named local variable in
the current scope.  It also contains `breakLoc` and `contLoc`, which are strings containing the
labels to break to or continue from when `break` or `continue` statements are encountered.

For most statements, `toAssembly` just finds the assembly template associated with it and returns an
empty `StringMap` along with the filled template.  The only time a `StringMap` is nonempty is if a
string literal is found within the statement.  Otherwise the `StringMap`s just merge and bubble up
to the top.  Any variable not found in `Vars` is assumed to be global and templated accordingly.

The `Templates` object stores all the necessary string manipulation required to generate the
assembly for each statement.  Some abstraction exists, but since large chunks of its operation will
(hopefully) be optimized out, it was not a priority.  The `Templates` object also contains a
function called `genSym`, which takes no arguments and returns a unique symbol to be used as a
label.  This is used almost entirely within the templates themselves to make labels for jumps.  The
only exceptions are string literals generating their labels and loops generating their `breakLoc`
and `contLoc` labels.

In these templates, `false` is represented by `0` and `true` by any value other than `0`.


Implementation Issues
---------------------

* Short-circuiting boolean `&&` and `||` was challenging in the LIR where everything was assigned to
  a value.  The flattening originally performed the following transformation:

        x = a && b || c     ->      t1 = a && b
                                    t2 = t1 || c
                                    x = t2

  If `b` and `c` were function calls, however, they would always be calculated, since the goal was
  to pre-calculate the values in steps.  The solution was to create an `if`-like structure that
  jumped across calculating further values if an operand of `&&` was false or one of `||` was true.

* Modulo didn't have an obvious primitive, but we eventually solved that by reading some x86
  documentation.  We use the remainder stored in `%rdx` after the `idivq` instruction.  This appears
  to be similar to gcc's handling of it, and we checked our implementation's behavior against that.

* The syntax of the `cmp` instruction was somewhat confusing to us, especially when inequality-based
  jumps had to be taken afterward.  It made it harder to debug the fact that my Lexer had the
  definitions of GT/LT and GTE/LTE flipped.  That issue has been fixed.

* Similarly to modulo, the `not` instruction we wanted didn't exist.  The unary `not` in x86 is
  two's-complement bitwise NOT, so it turns 0 into -1 and 1 into -2.  We wrote our own boolean NOT
  template and used that instead.

Known Problems
--------------

None yet.
