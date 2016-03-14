Semantic Checker
================


Division of Work
----------------

The initial division of work was for Jeremy to modify the grammar to make ANTLR output an AST and
Sorawit to start on the symbol tables.  Whoever finished first would take the semantic checks.
Sorawit ended up finishing first (and started on semantic checks), so Jeremy's additional task was
to write test files to exercise semantic checks not in the public tests.


Problem Statement Changes
-------------------------

The following are things that we specify in our semantic checking that are not explicitly stated in
the problem statement.

* A callout method can have any return value and any parameters, and it does not need to be
  consistent in the program.  For example, assuming `printf` is a callout, `printf(printf(true));`
  is legal.

* An IntLiteral that does not fit in 64 bits is considered illegal.  Note that both -2^63 and
  -(-2^63) are legal, as the IntLiteral itself (not the entire expression) fits in 64 bits.  It will
  overflow at runtime and the program will evaluate (-(-2^63)) to -2^63.  Any further negations will
  also result in -2^63.

* The program rejects accessing an array with a negative literal, meaning that it will not evaluate
  the expression.  This makes A[-1], A[---1], A[-f], A[-0x1] illegal, but keeps A[1], A[2-3],
  A[-5*1], and A[--1] legal.  In the case of A[---1] vs. A[--1], we know that the expression (-(-1))
  will evaluate to 1 and (-(-(-1))) to -1, so we can use those pre-evaluated values in semantic
  checks.

* With the previous two clarifications, A[-9223372036854775808] (a.k.a. A[-2^63]) is illegal due to
  array access with a negative literal.  A[--2^63] is illegal because we know that the expression
  (-(-2^63)) will result in a negative array access.  In a sense, when doing semantic checks, we can
  determine the sign of successively negated integer literals and use the eventual value in checks
  because it alerts the user earlier.

* The `main` method can have any return signature: void, boolean, or int.


Design
------

#### Abstract Syntax Tree

The AST is generated entirely by extended ANTLR grammar rules in the parser.  Semicolons and
delimiters are omitted from the AST.  Most expressions, such as control-flow statements and binary
operations, have a natural head. In those cases, that head is used as the root of the subtree.
Other expressions, including declarations and parameter lists, have no natural head (especially the
ones that use Kleene stars and can actually be empty), so imaginary tokens are inserted into the
tree.  This allows AST nodes like `program` and `methoddecl` to contain the same number of children
every time they are encountered, in any program.

The AST-generating extensions combined with the parser rules themselves had the consequence of
making our binary operators right-associative.  This is unusual for C-like programming languages,
but we realized this late enough in the process that we decided to delay changing it until after
this project was completed.  It has no effect on our semantic checks, so the grammar can be replaced
whenever it is finished.


#### Intemediate Representation Design

The key design up the IR is that it will be represented by a tree, of which each node is an object
subclassing from class Ir.  Following is the class hierarchy and class specification of the Ir
classes.

* abstract class Ir(AST)
    * case class IrProgram([IrCalloutdesc], [IrFieldDecl], [IrMethodDecl])(AST)
    * abstract class IrDecl(AST)
        * case class IrCalloutDecl(String)(AST)
        * case class IrFieldDecl(VariableType, [IrVariable])(AST)
        * case class IrMethodDecl(VariableType, String, [(VariableType, String)], IrBlock)(AST)
        * case class IrBlock([IrStatement])(AST)
    * abstract class IrStatement(AST)
        * ...
    * abstract class IrExpr(AST)
        * ...
    * abstract class IrParam(AST)
        * ...
    * abstract class IrLocation(AST)
        * ...

As we can see, each Ir has an AST that corresponds to itself.  Also, each subclass (both abstract
and non-abstract) of Ir comes with factory method generate() and check().  We will talk about these
two methods in the following sections.

#### IR Generation

To generate IR that corresponds to the given parse tree.  We call the factory method named
'generate' to the matched Ir class.  For example, the uppermost AST will match with IrProgram.  Then
the generate method will recursively create proper children of the current Ir node by calling
generate() on the AST subtree, which will return another Ir node.  Most of the work here is done
using Scala's pattern matching on the tree's tokenType().  Note that the pattern matching does not
contain wildcard match, and thus it can lead to runtime error if the given parse tree is not the
expected format.  However, given that the input parse tree is correct, it is not possible that the
runtime error will occur during the IR generation.  The IR generation part extensively uses several
helper methods.  These methods can be found in util/Helper.scala.

#### Semantic Check Workflow

All Ir nodes come with a method check() that takes in a current symbol table, and returns a tuple of
updated symbol table and list of all detected errors.  Therefore, in the outermost call, we simply
call check() with initial symbol table, and take the result symbol table and error list.  The
semantic check will pass if the error list is empty and the result symbol table contains a method
main with no parameters.

Under the hood, each check() will first check the semantic error of the node on which it was called.
For example, if the node is an Assign Statement, it does variable lookup in the symbol table and
sees if it is legal to assign the variable.  If necessay, it might call getType() on its nested
expressions.  After that, it creates a new symbol table using the given symbol table, recursively
calls check() on its sub-nodes, and collects all the errors.  Lastly, the check() method returns the
updated symbol table with the list of all the collected errors.

#### Expression Type

Every IrExpr comes with a method getType() that returns an option of its type, which can be None,
Some(VariableInt), Some(VariableBoolean), or Some(VariableCallout), as well as an option of error,
which can be None or Some(error).  We decided to use option instead of list here because we do not
want to report all the errors related to an expression, but rather only the innermost one.  Note
that the return type of None can mean some error occurs, or the type is not int, boolean, or callout
(which means it is a char).

Under the hood, the getType() function returns the type if the expression is some kind of literal.
Otherwise, it performs the getType() of its nested expressions, and figures out its own type.  If
some error occurs, it propagates the error.  Otherwise, it returns its own type.  For example, (1 +
2) will return Some(VariableInt).

#### Atomic Token

We use Scala's case object as a representation of each Ir's token.  We use the case object becase it
allows direct comparison, and we do not need to instatiate the object at all (it works like an enum
in that sense).  We group several objects of the same kind with trait.  Following is the
(abbreviated) hierarchy of expression types and Atomic Tokens used in the semantic checker.

* trait VariableType
    * VariableInt
    * VariableBoolean
    * VariableCallout
* trait BinopIntType
    * BinopPlus
    * BinopMinus
    * ...
* trait BinopBooleanType
    * BinopGT
    * BinopLT
    * ...
* trait BinopEqType
    * ...
* trait BinopAndOrType
    * ...
* trait AssignType
    * ...

#### Symbol Table

A symbol table is implemented using a case class that contains a ScopeType and two maps whose keys
are strings and whose elements are objects that subclass Identified. A ScopeType is one of the
following: ScopeGlobal, ScopeMethod, or ScopeLoop.  ScopeMethod and ScopeLoop also contain the
VariableType of the current working method, so that if we encounter a return statement, we can check
if the return expression's type is legal.

A symbol table contains two maps, each linking identifier names to their corresponding types
(Scalar, Vector, Method, or Callout).  One symbol table is used to keep track of anything in the
current scope, and the other is used for outer scopes.  This allows us to cleanly implement the
shadowing of variables.

A symbol table is designed to be an immutable data structure, and therefore contains several
producer methods:

* addSymbols: returns a new symbol table with a new pair of id -> type
* addLocal: returns a new symbol table with all method parameters added
* chScope: changes the scope of the symbol table, and moves all data in the second map onto the
  first map.
* toLoop: changes the scope to the loop, but preserves the return type of the scope.


Implementation Issues
---------------------

#### Similarity and Difference between Callout and Void

Decaf is a statically-typed language, so we specify that any expression (subclass of IrExpr) has to
have a method getType(), which returns its type (and any nested semantic errors). The problem
occured when we assumed that the callout returns nothing and thus has a type of None.  The
expression type of a void method call was also None.  However, those two Nones are different.  For
example, assume p is a callout method while v is a void method, `2 + p()` is legal while `2 + v()`
is not.  To solve this problem, we created another VariableType in addition to VariableInt and
VariableBoolean, and we called it VariableCallout.  This VariableCallout can basically present
itself as any possible VariableType.

#### Error Line Number not Showing up Properly

One bad thing of using the official antlr2.7 is that it discards the line number information of the
token when generating the parse tree (CommonAST).  Therefore, when reporting semantic errors, we
could not get the line number to show up as anything other than zero.  To solve this problem, we dug
into the source code of antlr2.7, made some changes to the CommonAST initlaize methods, and
recompiled it.  After doing this, we could successfully get line numbers in the parse tree, but only
in a node that corresponds to a token generated in the Lexer.  (Our parser generates some tokens
like BLOCK, METHOD_PARAMS, and FIELD_DECL that don't have a natural tree head.)  In order to make
all nodes contain line information, we perform a BFS on the tree, and propagate the line number
information of the earliest-encountered child to its parent in the case that its parent has no line
information.

#### Passing the AST around in the IR Nodes

Because each node in the IR needs to be able to report (in the case of a semantic error) the line
and the column of the problem, each Ir object needs to be able to reference back to its
corresponding AST.  To do this, we explicitly pass the AST subtree around in the factory method of
each Ir.  However, this makes the code hard to maintain, as the code looks noisy.  To solve this
problem, we instead specify the Ir class to take an implicit parameter called `ref`, which will
represent its corresponding AST.  Now, the AST gets passed automatically when initializing any Ir.


Known Problems
--------------

None, yet.
