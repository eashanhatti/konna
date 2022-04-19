# Konna

An experimental language for exploring the practical applications of two level type theory.

### Introduction

A common pattern in languages with performance as a goal is to have some features be compile time only ("static"). C++ templates for instance, eliminate the overhead of polymorphism by statically generating specialized versions of each templated definition. However, another common trend is for these compile time languages - at the start very simple - to accrue more and more features that interact in ad hoc ways, harming ergonomics.

The problem here is not with the idea itself, but rather the execution. These static languages aren't designed from the start to be *actual languages*. Konna has a static language and a runtime language, but the static language is fully featured from the start. The static language is dependently typed, pure, and can manipulate programs of the runtime language as data. In contrast, the runtime language is monomorphic and impure. The combination of these two allows features such as conditional compilation, templates, user-directed inlining, compile time evaluation, macros, and specialization of DSLs to be expressed very naturally.

The language is a WIP, so there's numerous nontrivial bugs. However, currently the only barrier to writing "real" programs in it is the lack of IO.

### References and Inspirations

A list of prior art that have influenced Konna's design and implementation can be found [here](https://github.com/eashanhatti/konna/blob/master/REFERENCES.md).

### Examples

The type of runtime-relevant pairs. Product types are tuples, not records - fields are not named.
```
product Pair : (a:U0) -> (b:U0) -> U0
    a
    b
```
Erased natural numbers and the unit type.
```
datatype Nat : U1
    zero : Nat
    succ : Nat -> Nat

product Unit : U0
```
An array type, implemented as nested pairs.
```
val Array : Nat -> U0 -> U0 = case
    zero _ => Unit
    (succ n) a => Pair a (Array n a)
```
A `map` function for arrays. `<_>` indicated quoting, `~_` indicates splicing, and `Code _` is the type of a quoted code fragment. `#T` indicates the construction of or pattern matching on a product value.
```
val map : (n : Nat) -> (a b : U0) -> Code ((a -> b) -> Array n a -> Array n b) =
    λn a b => case n
        zero => <λf arr => case arr
            #Unit => #Unit>
        succ n => <λf arr => case arr
            #Pair x arr => #Pair (f x) (~(map n a b) f arr)>
```
Implicit arguments, function definition syntax, and implicit splicing/quoting/`Code` insertion have not been implemented, hence why this example is rather cluttered. Here's what it would look like with those features:
```
fun map {n : Nat} {a b : U0} (f : a -> b) (arr : Array n a) : Array n b = case
    f #Unit => #Unit
    f (#Pair x arr) => #Pair (f x) (map f arr)
```
More pattern matching.
```
val max : Nat -> Nat -> Nat = case
    zero m => m
    n zero => n
    (succ n) (succ m) => succ (max n m)
```
The bool type, a predicate on bools, and dependent pattern matching.
```
datatype Bool : U1
    true : Bool
    false : Bool

datatype IsTrue : Bool -> U1
    is_true : IsTrue true

val foo : (b : Bool) -> IsTrue b -> ? = case
    true is_true => ?

(* Fails to typecheck. Error messages are pretty ugly at the moment, but the core
   of the error says "Expected pattern of type `IsTrue false` in second pattern
   of `case` expression, but pattern `is_true` is of type `IsTrue true" *)
val bar : (b : Bool) -> IsTrue b -> ? = case
    false is_true => ?
```

### Planned Features

* IO
* Compilation to native code
* Extensible datatypes
* Arity polymorphism for higher order functions
* Linear/uniqueness types
* Stage polymorphism
