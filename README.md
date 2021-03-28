# About
Ivy is a small functional programming language, defined by a set of operational semantics.
This is an Ivy interpreter, written in haskell. Ivy has the following generic features:
+ it is lazy (call-by-name)
+ it is pure (no side-effects)

# Semantics
I use an operational semantics for the language, being as it is relatively easy to interpret/generate. The general form is to define a set of expressions, and a global store. Then, define a reduction relation on these expressions. I *also* define processes, which can be thought of as a superset of expressions, allowing sequencing and changing the global store.

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

## Programs/Processes
The Ivy language defines *processs* to consist of either expressions, definitions or sequences of other processs,
i.e.

p ::= e | p1 p2 | (def x e)

This means that 
```
(def x 1)
(def y 2)
```
is a process, but
```
(if <condition> (def x 1) (def y 2))
```
is not, i.e. Seqencing and definition can only occur at the 'top level'. 

Definition: define-by-name: if a value is never needed, it is not evaluated.

![](doc/imgs/progn-def.png?raw-true)

Sequencing

![](doc/imgs/progn-discard.png?raw-true)

![](doc/imgs/progn-seq.png?raw-true)

Substitution - the definition attempts to make it so that substition only applies if:
(a) a program is stuck.
(b) there are free variables in the stuck program.

![](doc/imgs/progn-subst.png?raw-true)

# Some Examples
2 + 3 + 4:
```lisp
(+ 2 (+ 3 4))
```

The factorial function:
```lisp
(fn fact x 
    (if (>= x 1) 
        (* x (fact (- x 1))) 
        1))
        
;; alternatively
(def fact (fn x
   (if (>= x 1)
       (* x (fact (- x 1)))
       1)))
```

Functions can take only one argument. For a 'two-argument' function, we use nested functions:
```lisp
(def sum (fn x (fn y 
  (if (>= x y) x y)))) 

((sum 3) 4)
```
