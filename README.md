# Future: Gradually typed, upgradable Lisp

**Warning:** this project is in its EXTREMELY early stages. It doesn't even have a working interpreter yet. This repository exists mostly just as a place to store my ideas, code and research.

Future is a gradually typed Scheme variant, with Clojure-inspired syntax, immutability and support for first-class types.

## Examples

Future is built on a subset of primitive Scheme data types and operations

```
x           ; :Symbol
4           ; :Int
1.50        ; :Float
22/7        ; :Ratio
"hello"     ; :String
\c          ; :Char
true        ; :Bool
'(1 2 3)    ; :List
[1 2 3]     ; :Vector
{:a 1 :b 2} ; :Map
(1 . 2)     ; :Pair
#(+ %1 %2)  ; :Func
```

Functions are dynamically typed when written without type declarations...

```
(defn greet (name)
    (++ "Hello " name "!"))

>>> (:Func '(:?) :?) (fn (name) ...)
```

...but statically typed when written with them.

```
(defn add (:Int x :Int y) :Int
    (+ x y))

>>> (:Func '(:Int :Int) :Int) (fn (x y) ...)
```

Values can be partially or fully typed

```
(:List '(1 2 3))
>>> (:List :?) (:Int 1 :Int 2 :Int 3)

((:List :Int) '(1 2 3))
>>> (:List :Int) (:Int 1 :Int 2 :Int 3)

((:List :Int) '(\a \b \c))
>>> Type error - expected :Int, found :Char
```

In addition, Future also supports defining custom data types...

```
(deftype (:Maybe x)
    (just x)
    (nil))

>>> :Type (:Maybe :?)

nil
>>> (:Maybe :?) nil

(just 3)
>>> (:Maybe :?) just 3
```

...as well as the ability to use types as first-class values.

```
(defn check-type (:Type t v)
    (= t (type v)))

>>> (:Func '(:Type :?) :?) (fn (t v) ...)

(check-type :Int 3)
>>> :Bool true

(check-type :Char 7)
>>> :Bool false
```

## Planned Features
- Whole-program optimizing compiler
  - Uses GRIN as IR
  - Aggressive inlining, defunctionalization and unboxing
  - Aggressive dead code elimination
  - ASAP memory management
  - SIMD vectorization of lists
- Transactional, atomic live upgrade of code and data within a running program
- Actor-style concurrency

## Papers
- [Flow-Directed Lightweight Closure Conversion](./papers/fdlcc.pdf)
  - Describes efficient closure conversion using interprocedural analysis
- [Houyhnhnm Computing Chapter 5: Non-Stop Change](https://ngnghm.github.io/blog/2015/09/08/chapter-5-non-stop-change/)
  - Describes design goals in regards to language-level support for data and code upgrading
- [Pushdown Control-Flow Analysis for Free](./papers/1507.03137.pdf)
  - Easy to implement, inexpensive control-flow analysis
- [Compiling Tree Transforms to Operate on Packed Representations](./papers/LIPIcs-ECOOP-2017-26.pdf)
  - Describes how to compile code that operates on trees to operate on packed representations of trees
- [ASAP: As Static As Possible memory management](./papers/UCAM-CL-TR-908.pdf)
  - ASAP memory management strategy; needs to be extended to support higher-order functions
    - Could be alleviated with defunctionalization
- [Gradual Typing for Functional Languages](./papers/13-siek.pdf)
  - Gradual type system for lambda calculus
- [Putting Gradual Types to Work](./papers/2101.12299.pdf)
  - Examples of what is possible with gradual typing
- [Gradual Types with Unification-based Inference](./papers/dls08igtlc.pdf)
  - Novel, efficient and reliable method of type inference for a gradual type system
- [Relational Type Theory](./papers/2101.09655.pdf)
  - Types are described as relations between terms; can do induction without dependent types
- [Automatic SIMD Vectorization for Haskell](./papers/vectorization-haskell.pdf)
  - Automatic SIMD vectorization of immutable arrays
- [A modern look at GRIN, an optimizing functional language back end](./papers/main.pdf)
  - Summary of GRIN IR and its current implementations
- [Composable Transactional Memory](./papers/2005-ppopp-composable.pdf)
  - Details on implementing STM, useful for concurrency implementation

## Resources
- [Write Yourself A Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)
  - Main resource for building the first version of the interpreter
- [Write You a Haskell](http://dev.stephendiehl.com/fun/index.html)
  - Includes useful information about building functional language compilers
- [Souper: Superoptimizer for LLVM IR](https://github.com/google/souper)
  - Useful for implementing new optimizations in the GRIN compiler
