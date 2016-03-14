Dataflow Analysis
=================


Division of Work
----------------

Since there were two phases we each took one.  Sorawit converted the low-level IR into a control
flow graph (LIR -> CFG), and Jeremy wrote the Common Subexpression Elimination (CSE) code to modify
the CFG.  We also had to revisit code generation output directly from the CFG.  Since it was mostly
minor changes, we both worked on them.


Problem Statement Changes
-------------------------

* Functions are assumed to be universally impure and are never considered eligible for elimination.

* Global variables are assumed to be universally impure (since global state can change after
  function calls) and are never considered eligible for elimination.

* Array varialbes are considered avaiable-or-not as an entire array, not on an individual
  index. This is because sometimes there might be statements such as `a[i] = 10`, which doesn't tell
  us exactly which index is being mutated.


Design
------

### LIR -> CFG

Our design converts each function's representation, which is currently an LIR, to a graph
representation.  A graph is a case-class containg a starting node and some meta information of the
function, including the parameter list, local variable list, and function name.

A node is a recursive datatype. Each node contains a list of executing statements and optional
values of one or two nodes that are its successors. In the case that there are two, it also includes
the variable that determines which one to take.

To generate a CFG of each function, we simply traverse the LIR and construct nodes as needed.  Each
statement / block of the LIR implements a function `toCfg()` which will output its corresponding
contructed node. Note that this function takes two parameters, which are the nodes that the current
node must point to if a break / continue statement is encountered.

### CSE

The CSE code is part of the `optimize` package.  It's `apply` function is the main block of code
that takes a CFG, analyzes it, and eliminates common subexpressions.  First, it transforms the graph
by adding temporary CSE variables that store the result of every eligible expression.  Then, it
performs an available expressions analysis to determine which expressions are avaliable at each node
of the graph.  Finally, it transforms the graph by replacing available expressions by their
previously-inserted CSE locations.

Everything in CSE works on `Expression`s (canonicalizing expression structures), `String`s
(representing variables), and `cfg.Statement`s (containing codegen functions).  Expressions are
further broken down into types of expressions, that each track which variables they use.

The first transformation phase (inserting CSE temp variables) associates a (guaranteed-unique)
symbol to each expression eligible for replacement and inserts a node representing `cse_temp = expr`
into the CFG after the expression is calculated.  If a symbol already exists for an expression, the
temp variable is reused.  This guarantees that the variable containing the expression has not been
overwritten when we want to replace an available expression later.

The middle section of the code analyzes the transformed CFG for available expressions.  It first
grabs all expressions that exist in nodes and sets up functions to calculate `GEN`, `KILL`,
`predecessors`, and `successors` for each node.  `IN`, `OUT`, and `changed` datastructures are
initialized, and the rest of that block is the standard dataflow analysis algorithm for available
expressions.

The final chunk of code in CSE transforms each node to reusing available expressions.  If a
statement in a node uses an available (and replaceable) expression (`x = expr`), that line is
replaced with one that reuses the CSE temp variable (`x = cse1`).  Since this is only done for
expressions we've "cached" in the first phase, we don't try to replace function calls.


Implementation Issues
---------------------

We had to re-implement codegen to work on CFGs instead of the LIR, but that was just ASM templating
and wrestling with Scala to avoid copy-pasting whole methods.


Known Problems
--------------

None yet.  Unless you count the obscene number of temporary variables that now exist.  But those
will disappear after we implement DCE.
