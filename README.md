# Nomad
A CLI calculator application written in Haskell.
This project is a continued version of https://github.com/dhansmair/nomad-calculator (university project).
It's a learning project that explores functional programming in Haskell, but also other topics such as parsing, the lambda calculus and basic type inference.

Features of nomad:
- evaluate common arithmetic expressions
- Definition of variables and (mathematical) functions
- Partial function application
- higher-order functions 
- Builtin functions such as sin, cos, tan, log, exp etc.
- Builtin ```derive```, which returns the derivative of a function of type ```num -> num```.

Features that would be nice (but will probably not come):
- automatic simplification of terms and functions
- boolean type
- loading / executing code from file
- useful type error messages


## Setup
```
stack build                 // builds all executables
stack run cli               // runs cli executable
```

## Syntax
```
x = expr                    // define global variable
f(x,y) = expr               // define function
f = \x,y -> expr            // equivalently, functions can be defined as abstractions
expr                        // evaluate expressions
(\x -> 2*x)(4)              // lambda application
f(1,2)                      // function application
```

expressions can be:
```
1                           // any number
x                           // a variable
1 + 2 / 3 etc               // binary operations
f(x)                        // function call
(\x,y->x*y)(5,3)            // application of an abstraction
```

## Commands
```
:q                  close the cli
:t <expr>           show the type of an expression
:env                show the current environment (stored functions and variables)
```
