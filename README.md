# Ensemble

Ensemble is a prototype of a distributed, convergent, set-based
programming language inspired by APL.  Ensemble is build on top of the
[Lasp](http://github.com/lasp-lang/lasp) epidemic broadcast runtime
system for CRDTs.

<img src="https://travis-ci.org/cmeiklejohn/ensemble.svg" />

## Operations

Assign a set of values to a variable.

```
> A <- 1 2 3 4
{1 2 3 4}
```

Print the value of a variable.

```
> A
{1 2 3 4}
```

Map the set contents using a binary function like addition with a value
and assign the results to another variable.

```
> B <- A+1
{2 3 4 5}
```

Or, try multiplication.

```
> C <- A*2
{2 4 6 8}
```

You don't need to assign it either, it can evaluate like any other
expression in the language.

```
> A*2
{2 4 6 8}
```

Create a new set using the `iota` operation that will generate a series
of values; here, the set of values is from 1 to 10.

```
> D <- i10
{1 2 3 4 5 6 7 8 9 10}
```

Semi-colons can also be used to seperate statements on the same line.

```
> A <- i10; A
{1 2 3 4 5 6 7 8 9 10}
```

## Coming Soon

* Foldr
* Union
* Intersection
* Compliment
* REPL
* More!
* How to express operations like map by themselves.
* Is assignment really just an expression, or is it a statement?

## Copyright

Copyright 2016 (c) Christopher S. Meiklejohn.
