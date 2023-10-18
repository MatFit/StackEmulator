# StackEmulator
Arithmatic expressions into stack machine bytecode. Essentially, we create an abstract representation of arithmatic expressions. And also comes execution.

# What it is:
Given classes for arithmatic and boolean expressions and let bindings in some abstract grammar,
we aimed to compile this into digestable abstract representation of stack machine bytecode with
the use of said grammar "Expr". We are given an operand stack that containes values and runtime
stack containing a tuple of strings an values mapping identifies to current bindings. This is used
to deal with execution.

Overall premise of this was to understand how to recursively create abstract machine code
and pattern matching/rule making. The latter is super fundamental to the structure of code 
syntax. 
