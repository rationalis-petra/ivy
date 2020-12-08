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
![](doc/imgs/operations.png?raw=true)

### Conditions
Condition takes a step
![](doc/imgs/cond-step.png?raw=true)

condition true
![](doc/imgs/cond-true.png?raw=true)

condition false
![](doc/imgs/cond-false.png?raw=true)

### Functions
left-evaluation
![](doc/imgs/fun-left.png?raw=true)

application by name
![](doc/imgs/fun-app.png?raw=true)

recursive function expansion: x is the name of the function, and y is a formal parameter.
![](doc/imgs/fun-recursive.png?raw=true)

# Some Examples
2 + 3 + 4:
```lisp
(+ 2 (+ 3 4))
```

The factorial function:
```lisp
(fn fact x (if (>= x 1) (* x (fact (- x 1))) 1))
```

Take two values: x and y, return max(x, y). Apply it to arguments 3, 4
```
(((fn x (fn y (if (>= x y) x y))) 3) 4)
```

# Future Features
- [ ] Globally scoped functions
- [ ] Functions of multiple values
- [ ] Algebraic Data Types
- [ ] Macros
- [ ] Concurrency

