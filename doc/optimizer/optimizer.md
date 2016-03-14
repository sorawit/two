Optimizer
================


Division of Work
----------------

The major optimizations that we made in this phase are Common Subexpression Elimination, Copy
Propagation, Dead Code Elimination, Template Hacks, and Register Allocation.  Sorawit implemented
Register Allocation, and Jeremy implemented the other optimizations.  In addition, each of them
individually tried some other optimzations.


Problem Statement Changes
-------------------------

None.


Design
------

### Overall Design

We created a package in the project called `optimize`.  This package contains serveral static
objects, each of which takes in a Control-Flow Graph (so called CFG) and mutate the graph according
to its optimization.  From profiling the speedup.  We ended up doing CSE, CP, DCE until the code
reachs a fixpoint.  After that, we perform one Register Allocation optimization and codegen the
result.

Furthermore, after doing some number of optimizations, we periodically execute `cfg.refine` in order
to remove empty nodes in the graph, and clean up unreachable nodes.

To turn on each optimization, one can manually use flag --opt={cse, cp, dce, ra}, or --opt=all for
all optimizations.

### Optimizations Implemented

We implemented CSE, CP, DCE, and RA.  We also hacked our ASM templates a bit in order to save
instructions.

#### Common Subexpression Elimination

This is a cleaned-up version of the dataflow analysis used in the previous phase of the project.  It
took blocks like the code on the left, and transformed it into the code in the middle.  In practice,
it actually becomes the code on the right.

            x = 0                    x = 0                    x = 0
            y = x + 2                y = x + 2                t1 = x
            z = x + 2                z = y                    y = t1 + 2
                                                              t2 = y
                                                              z = t2

In the usual getting-worse-before-it-gets-better fashion, this makes more instructions, but CP and
DCE will eventually reduce them again.  Either way, there is only ever one addition computed, so we
save on arithmetic instructions.

The algorithm used is the available expressions dataflow analysis.  We first do one pass over the
CFG, adding new statements that save the results of computations to variables called `cseN|M` (where
`N` is the count of optimizations performed so far, and `M` is the count of variables from this
optimization pass).  This avoids namespace collisions.  We find the available expressions, and
perform another pass on the augmented graph replacing available expressions with their cached
locations (that have been saved from earlier).  We do not consider any expression containing a
function call, global variable, or array element safe to cache, so we never consider those
available.  A final pass is done to remove all unused temporaries from this CSE pass to avoid node
explosion (and remove useless instructions).

The returned values of the function are a boolean signifying whether this CSE pass changed anything
and the debugging output string.

#### Copy Propagation

This is a dataflow analysis algorithm that eliminates excessive copying instructions.  An example:

            x = 0               x = 0
            t1 = x              t1 = 0
            y = t1 + 2          y = t1 + 2
            t2 = y              t2 = y
            z = t2              z = t2

This optimization turns a `mov` instruction (possibly involving a memory load and store) into a
single store of a constant.

The algorithm used begins with a first pass over all the nodes to determine the identities of all
the definitions in the CFG.  It then runs a reaching definitions analysis on the graph.  If a
definition is not considered to define a variable to a constant (`x = 1` or `x = true`), it is not
considered in the analysis.  A final pass over the graph replaces any statements that copy a
variable defined to be constant if all reaching definitions of that variable contain the same value.

The returned values of the function are a boolean signifying whether this CP pass changed anything
and the debugging output string.

#### Dead Code Elimination

This is a dataflow analysis algorithm that eliminates definitions that are never used later in the program.

            x = 0
            t1 = 0               t1 = 0
            y = t1 + 2           y = t1 + 2
            t2 = y               t2 = y
            z = t2               z = t2

This optimization removes entire chunks of unused code from the CFG.

This algorithm uses liveness analysis, which begins from all exit nodes and proceeds to the start
nodes.  We did not implement a single exit node in our CFG, but the specification of the language
states that all functions must return a value or produce a runtime error (and we add returns to void
function accordingly), so we use all `return` statements as our set of exit nodes.  The first pass
over the nodes returns this set.  After liveness analysis is performed, a final pass over the graph
removes all dead assignments except those deemed un-removable.  These persistent statements are
assignments to globals or arrays, function calls, and return statements.

The returned values of the function are a boolean signifying whether this DCE pass changed anything
and the debugging output string.

#### Register Allocation

One of the simple examples of Register Allocation is shown in the following code snippet.

pseudo-code
    a = t + 2
    // never use t anymore

unoptimized code
    mov -8(%rbp), %r10
    mov $2, %r11
    add %r10, %r11
    move %r11, -16(%rbp)

optimized code
    // assume there is a register avaiable
    add $2, %r12

From this part of the code, notice that we can rename a variable "a" to be "t" without having any
problem, as "t" is dead after that point.  This leads to a huge reduction in number of instructions,
specifically the expensive load instructions.

To perform Register Allocation, we first perform avaliable expression and liveness analysis on the
CFG.  Then, we created Webs of def-use pairs, and used Graph Coloring heuristic provided in class to
rename all the variables.  We tried to minimize number of colors used, and assigned some colors to
registers.  All def-use pairs and are assigned to register colors can store their values in register
without having conflicts.

The other part was to modify the function call, so it saves all active registers to the stack before
performing the function call.  This prevents other function from modifying the caller's variables.

From profiling the result, we got significant improvment from this optimization.  Specifically, the
provided benchmark programs ran approximately 1.3-1.4 times faster than the unoptimized.  This is
due to the fact that we reduced a lot of memory fetches.  Register allocation also works well in
conjunction with all the other optimization, as the CSE, CP, DCE together get rid of unnessary
variables, allowing Register Allocation to use fewer colors.

This optimization is definitely general, as it does not change or reorder any part of the program.
Instead, it focuses on reusing variables and using the analysis to show they cannot be used at the
same time.  It also preserves semantics as it never changes the struture of the CFG nor the way
semantic checks are performed.

#### Template Hacks

Our assembly templates were originally designed for correctness and ease of debugging.  At the time,
we had constrained ourselves to only using stack locations for variables, so we had lots of
instruction bloat.  These templates now determine if the locations they use are registers and write
different assembly accordingly.  For example, the code on the left was generated with the old
template and the code on the right by the new template:

            mov %r14, %r10                add %r13, %r14
            mov %r13, %r11
            add %r11, %r10
            mov %r10, %r14

The new templates check to see if they are using registers instead of memory locations and don't
move arguments to temporary registers unless necessary (like `%rcx = %r14 - %r13`, since register
contents should not be modified here).  If the result location (also referred to as left-hand side)
is the same as one of the arguments (and is a register), an abbreviated template is used to remove
the need for a temporary variable.  Additionally, since we only deal with booleans and integers
(whose addition and multiplication are commutative), we can reverse the order of arguments if that
is useful.

In comparison operations, if reversing the direction of comparison produces faster code, the
comparison operator is switched along with the arguments.  We found that our assembler required a
register in the second argument to `cmp`, and this was our way around it.

Some templates were simply bloated anyway (such as `not` requiring two extra `mov` instructions),
and these were tweaked for instruction count.

Finally, some templates (specifically those for the modulo and division operations) were adjusted to
allow for more registers to be available to register allocation.


### Optimizations Not Implemented

#### Function Inlining

We spent a few days working on implementing this feature, as we believe that Register Allocation
wastes so much time pushing and popping registers to the stack when calling nested functions.  Thus,
inlining the function could potentially avoid those pushing-popping problems.  This is the example
of inlining optimization.

pseudo-code
    int double(int x) { return 2 * x; }
    ...
    y = double(3)

unoptimized code
    push ALL REGISTERS
    mov -8(%rbp), %rsi
    call double
    pop ALL REGISTERS
    mov %rax, %r13

optimized code
    mov -8(%rbp), %r11
    mul $2, %r11
    mov %r11, %r13

It turned out that this optimization did not improve the performance of the program by a lot.  We
did not see any significant improvements from it, both when running only inline and when running
inline with other optimizations.  One of the reasons we can think of is that inlining functions
noticably increases number of local variables in a function, and this makes it harder for Register
Allocation to color those def-use pairs of variables.  The solution to this problem might be to do
web-splitting to split each webs if there is an inline function in the web.  We, however, think that
this solution will overcomplicate the design of Register Allocation, potentially leading to
undeterministic bugs that are not easy to catch.  Therefore, we decided to drop this optimization.

Additionally, function inlining made the compilation of the program extremely slow, as it
dramatically increases the number of nodes to analyzy by combining those from multiple functions
into one graph.  All other optimizations depend heavily on graph traversal, so they all suffer from
node explosion with the inlining optimization turned on.

#### Loop Unrolling

Although loop unrolling would probably provide a large benefit with the benchmark programs
available, we decided that register allocation was probably a better choice.  Given more time, this
is probably the next optimization to implement.  Many programs spend most of their time in loops,
but it may require some changes to register allocation.

#### Instruction Scheduling

We thought that Instruction Scheduling could have big impact on the performance, as it reduces the
number of wasted cycles when running a compiled program.  However, we spent most of our time
debugging and ensuring that other optimizations are properly imlemented.  Therefore, we decided to
drop this optimization, as we believed it is not worth the time and effort to implement and debug.


Implementation Issues
---------------------

* Freeing up enough registers for RA.

  We tried to maximize the number of registers we are allowed to use safely.  It turned out however,
  that several registers are reserved for various purposes.  For instance, `%r10` and `%r11` are
  reserved for computations (by our templates), and `%rsi`, `%rax`, `%rdi`, etc. are for function
  calls.  We worked around this by pushing and popping those registers at right times, so they do
  not conflict with other uses.  This, however, led to a lot of load-write instructions, and may
  suffer a performance hit.

* Debugging the program.

  The bugs that we faced were sometimes non-deterministic.  This is due to our heuristic function
  (for register allocation) containing uses of randomness, specifically `hashCode`.  This made it
  hard to reproduce bugs once we found them.  We however, decided to leave the program not totally
  deterministic, as we think that this behavior made it easier for bugs to appear, so we can fix
  them.  Additionally, it makes the algorithm simpler, which is better than being completely optimal
  in our case.


Known Problems
--------------

None, yet.
