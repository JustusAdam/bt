---
author: Justus Adam
title: Ÿauhau - Optimised IO from Dataflow
subtitle: >
    TU Dresden
    Chair for Compiler Construction
date: 07-15-2016
---

## Title for motivation

```Haskell
commonFriends :: Id -> Id -> IO [Id]
commonFriends x y = do
    fx <- friendsOf x
    fy <- friendsOf y
    return (intersection fx fy)

friendsOf :: Id -> IO Id
```

- lots of IO for a simple calculation
- sequential IO

---

```Haskell
commonFriends :: Id -> Id -> IO [Id]
commonFriends x y = do
    batched <- batchRequest [FriendsOf x, FriendsOf y]
    let [fx, fy] = map unpackResponse batched
    return (intersection fx fy)

data Request = FriendsOf Id | UserPage Id | UserAge Id | ...

batchRequest :: [Request] -> IO [Response]
```

- what about batching across functions?
- what about concurrency?

## Target domain

- large scale, distributed systems (Facebook, Twitter)
- infrastructure built on services

## Table of content

<ol>
<li><a href="#/prior-work">Prior work</a></li>
<li><a href="#/ÿauhau-and-ohua">Ÿauhau and Ohua</a></li>
<li><a href="#/context-and-control-flow">Context and control flow</a></li>
<li><a href="#/unwinding-context-for-ÿauhau">Unwinding context for Ÿauhau</a></li>
<li><a href="#/remaining-work">Remaining work</a></li>
</ol>

# Prior work

## [Haxl](https://hackage.haskell.org/package/haxl)

```Haskell
commonFriends :: Id -> Id -> Fetch [Id]
commonFriends x y = intersection <$> friendsOf x <*> friendsOf y

friendsOf :: Id -> Fetch [Id]
friendsOf uid = dataFetch (FriendsOf uid)
```

- [Haskell](https://haskell.org) library for automatic IO batching
- Developed by Facebook for spam fighting
- write simple and straight forward code and still get performant results
- uses [Applicative Functors](https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Monad.html)


## Muse

```Clojure
(defn common-friends [x y]
  (<$> intersection (frinds-of x) (friends-of y)))

(defn friends-of [x]
  (FriendsOf. x))
```

- [Clojure](https://clojure.org) library reimplementing Haxl
- Similarly relies on Applicative functors as DSL to build a custom AST

## Problems

. . .

- Unfamiliar syntax
    - For Haskell programmers using applicative, rather than `do`
    - For Clojure Applicative and Monads in general
- Suboptimal results, `>>=` and `do`-notation forces fetches

<div class="notes">
- Sidenote: `ApplicativeDo`
</div>

# Ÿauhau and Ohua

## Ÿauhau

```Clojure
(defalgo common-friends [x y]
   (intersection (friends-of x) (friends-of y)))

(defalgo friends-of [x]
  (fetch (mk-req x my-data-source)))
```

- System with similar goals to Haxl and Muse
- Also uses the Clojure language

---

### Write any code

```Clojure
(defalgo common-friends [x y]
  (let [fx (friends-of x)
        fy (friends-of y)])
   (intersection x y))
```

- independence from code style
- Ÿauhau's EDSL: `fetch`

---

### Under the hood

- Uses data flow graph transformations
- built on [Ohua](https://bitbucket.org/sertel/ohua)

## Ohua

- EDSL for clojure for automatic parallelism
- Stateful functions in Java or Clojure
- Clojure Algorithms
- Ohua parallelises algorithms and schedules stateful functions

<div class="notes">
- Low level, state manipulating routines in Java (Stateful functions)
- Stateful functions are atomic
- High level, pure algorithms in Clojure
</div>

## Emerging challenges

- Naive graph transformations don't consider control flow
- Accumulative fetches must receive inputs simultaneously
- Generic system for capturing control flow is necessary

# Context and control flow

---

Context is a property of subgraphs of a data flow graph emerging from node labels and not visible structurally.

## Properties

- Inherited
- Subgraphs enclosed in special `begin` and `end` node
- Contexts always completely enclose nested contexts

. . .

> A context does not spill

## Detection

<!-- Not sure this slide is needed -->

- Annotating nodes during compilation
- Building context map
- Graph transformations must provide context information for new nodes

# Unwinding context for Ÿauhau

---

- Main contribution of the Thesis
- Iterative graph transformations
    - Calculate context stacks
    - Unwind in order of descending depth
- Dispatch transformation based on context type

---

- General Idea: Close context before fetch, reopen afterwards
- Uses data structures to encode the context state
    - Trees for `smap`
    - Empty fetches for `if`

# Remaining work

## Write semantics

- Ensuring deterministic results when using writes
- Make algorithms dependent if writes are present
- Seq'ing algorithms

## Optimisations

- Context rewrites are kept simple, therefore wasteful
- Remove redundant operators
- Coerce operators like select

## Experiments

- Compare our if-rewrite to Haxl and Muse
- Evaluate performance of Smap rewrite
- Test `ApplicativeDo`
