# Future: Gradually typed, upgradable Lisp

**Warning:** this project is in its EXTREMELY early stages. It doesn't even have a working interpreter yet. This repository exists mostly just as a place to store my ideas, code and research.

## Features
- Functional programming
  - Pure functions & immutable data
  - Optional lazy evaluation
- Gradually typed, w/ first-class types
- Supports transactional, atomic live upgrade of code and data within a running program
- Whole-program optimizing compiler
  - Uses GRIN as IR
  - Aggressive inlining, defunctionalization and unboxing
  - Aggressive dead code elimination
  - ASAP memory management
  - SIMD vectorization of lists
- Actor-style concurrency

## Papers
[Flow-Directed Lightweight Closure Conversion](./papers/fdlcc.pdf)
  - Describes efficient closure conversion using interprocedural analysis
[Houyhnhnm Computing Chapter 5: Non-Stop Change](https://ngnghm.github.io/blog/2015/09/08/chapter-5-non-stop-change/)
  - Describes design goals in regards to language-level support for data and code upgrading
[Pushdown Control-Flow Analysis for Free](./papers/1507.03137.pdf)
  - Easy to implement, inexpensive control-flow analysis
[Compiling Tree Transforms to Operate on Packed Representations](./papers/LIPIcs-ECOOP-2017-26.pdf)
  - Describes how to compile code that operates on trees to operate on packed representations of trees
[ASAP: As Static As Possible memory management](./papers/UCAM-CL-TR-908.pdf)
  - ASAP memory management strategy; needs to be extended to support higher-order functions
    - Could be alleviated with defunctionalization
[Gradual Typing for Functional Languages](./papers/13-siek.pdf)
  - Gradual type system for lambda calculus
[Automatic SIMD Vectorization for Haskell](./papers/vectorization-haskell.pdf)
  - Automatic SIMD vectorization of immutable arrays
[A modern look at GRIN, an optimizing functional language back end](./papers/main.pdf)
  - Summary of GRIN IR and its current implementations

## Resources
- [Writing A Lisp Interpreter In Haskell](https://www.defmacro.org/ramblings/lisp-in-haskell.html)
- [Write You a Haskell](http://dev.stephendiehl.com/fun/index.html)
