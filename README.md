# About
Ivy is a small functional programming language, defined by a set of operational semantics.
This is an Ivy interpreter, written in haskell. Ivy has the following generic features:
+ it is lazy (call-by-name)
+ it is pure (no side-effects)

# Semantics
## Expressions
The Ivy Lanugage encompasses the set of expressions:

e ::= l | (if e1 e2 e3) | (fn x e) | (fn x y e) | (op e1 e2) | (e1 e2)

Where op ranges over the set of operators {+, -, *, >=} and l ranges over the set of literals: Integers U {nil, t}, and x are vairable names.

## Reduction Rules 
Where we let v be values, with v ::= l | (fn x e) | (fn x y e)

### Operations
![](ivy/doc/operations.png?raw=true)

### Conditions
Condition takes a step
![](ivy/doc/cond-step.png?raw=true)
condition true
![](ivy/doc/cond-true.png?raw=true)
condition false
![](ivy/doc/cond-false.png?raw=true)

### Functions
left-evaluation
![](ivy/doc/func-left.png?raw=true)

application by name
![](ivy/doc/func-app.png?raw=true)

recursive function expansion: x is the name of the function, and y is a formal parameter.
![](ivy/doc/func-recursive.png?raw=true)

# TODO
- [ ] Algebraic Data Types
- [ ] Macros
- [ ] Concurrency

