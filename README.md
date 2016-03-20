# Ensemble

Ensemble is a prototype of a distributed, convergent, set-based
programming language inspired by APL.

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

Map the set contents using a binary function and a value and assign
the results to another variable.

```
> B <- A+2
{2 3 4 5}
```

Create a new set using the `iota` operation that will generate a series
of values; here, the set of values is from 0 to 1000.

```
> C <- i10
{1 2 3 4 5 6 7 8 9 10}
```

## Coming Soon

* Foldr
* Product
* Union
* Intersection
* Compliment
* REPL
* More!
* Print should use proper set notation.

## Copyright

Copyright 2016 (c) Christopher S. Meiklejohn.
