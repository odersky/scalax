# The Capture Calculus

Variable capture is a big spoil-sport for program analysis since it
makes it hard to keep track of escaping references.  A value _escapes_ from some scope if it
can be referred to outside that scope. This could happen because
the value was assigned to some variable outside the scope,
or because the value is part of (i.e. reachable
from) the result of the scope. We will restrict ourselves here to functional programs and therefore look only at the second class of escapes. If we knew what other values formed part of
a value just by inspecting its type, finding escaping references would be
easy. But a value can also be _captured_ by a closure, if the
closure refers to it in some its free variables. In this case the
closure's function type would contain no mention of the escaping
value.

This is a pity, since if we could determine accurately what
values cannot appear in the result of an expression, we could apply
a number of interesting techniques:

 1. Allocate a value on the stack, if the value is does not form
    part of the result of the method that defines it.
 2. Or, allocate values in a separate region that is de-allocated
    in its entirety once we know that none of the values allocated in the region can escape.
 4. Or, make sure that only a single reference survives to an object after a point,
    so that the object can be mutated without the need for defensive copying (i.e. enforce linearity or some other ownership discipline).
 3. Or, complement a simple capability passing system to form a full effect system, where effects are modelled as capabilities and escaping references are checked to ensure that capabilities have bounded lifetimes.

_Escape analysis_ is a way to keep track of captured variables
(somewhat confusingly, variables _escape_ if they are _captured_ in a closure
that forms part of the result). Various forms of escape analyses
exist. So far they are mostly targeted to be a preparatory step for some
optimizations. So, an escape analysis could allocate a value opportunistically on the stack if it detects that it cannot escape from the method that defines it. But it would be much harder to support a region allocation scheme where a program is rejected if objects allocated in a region survive the region's termination point. Or, an escape analysis could eliminate
some redundant defensive copies when messages are sent from one actor
to another. But it cannot flag it as a compile-time error if part of a
sent message is referenced again by its sender, i.e. the message is
used non-linearily. And, escape analyses have not yet been used at all for
turning capabilities into effects.

To be able to support these additional requirements, an escape analysis would best come in the form of a type system, where (lack of) escapes can be specified by the user, hopefully in a way that is modular and readable. This note develops a type-theoretic foundation for such systems. We augment a language close to System-F with the abilities to track captured references and to postulate and check that some references
do not escape.

## Abstract Syntax

We study a language with the following abstract syntax:

    Variable       x, y, z
    Type variable  X, Y, Z

    Type     R, S, T  =  X                   type variable
                         (x: T) -> T         function
                         [X] -> T            type function
                         x |> T              T capturing x

    Value       v, w  =  (x: T) -> t
                         [X] -> t
    Term        s, t  =  v
                         x
                         t t
                         t[T]

The studied language is close to System F with one main addition: the type
`x |> T` augments the type `T` with the additional info that variable `x` might be captured. Values
of this type are all values of `T` that might contain a reference to `x`.
By contrast, the values of a type variable or function type `T` by itself are restricted to values that are guaranteed not to reference any tracked variable. Otherwise put, the values of a type `x1 |> ... |> xn |> T` where `T` is a type variable or function type are all values of `T` that reference no tracked variable other than `x1, ..., xn`.

A variable `x` is _tracked_ if it is introduced by a binding of the form `x: x |> T` (where the two occurrences of `x` are the same name).
Such a variable can be substituted with values that track arbitrary
other variables. In a sense, the tracking reference `x` subsumes all references in the values that get substituted for `x`.

Variables introduced by other bindings are not tracked. For instance a variable declared as `x: y |> X` is not itself tracked. Instead, it can be substituted by types that might capture the tracked variable `y`. Or, a variable declared as `x: A -> B` is a variable that is not tracked, and that can be substituted only with functions from `A` to `B` that do not capture any tracked variables themselves.

Functions and function types in the capture calculus _CC_ are written a bit differently from System F. The function value `lambda x:T.t` and type function `Lambda X.t` are written `(x: T) -> t` and `[X] -> t`, respectively (this is just a matter of presentation). The difference in the function type is a bit more fundamental: Instead of `X -> T`, we write `(x: X) -> T`, where `x`
names the bound parameter. This binding is needed since we might need
to refer to `x` as a tracked variable in `T`. Tracking is the only form
of term-dependency in our calculus (but more general term dependencies could be added). Types of polymorphic functions are written `[X] -> T`, mirroring polymorphic function values.



## Preliminaries

1 . The `->` and `|>` operators have the same precedence and they both associate to the right:

    R -> x |> S -> y |> T  =  R -> (x |> (S -> (y |> T)))

2 . We use an alternative way to represent a type `x1 |> ... |> xn |> T` as
`{x1, ..., xn} |> T`. The set `{x1, ..., xn}` is called a _capture set_.
We use the letter `C` to range over capture sets.

More formally, we define two functions that compute the capture-free core and the capture set of a type `T`:

      core(T) = core(T0)     if T = x |> T0, for some x, T0
              = T0           otherwise

         C(T) = {x} u C(T0)  if T = x |> T0, for some x, T0
                {}           otherwise

Structural term equality is given by the usual alpha renaming, plus the equality

      T1 === T2   if core(T1) = core(T2) & C(T1) = C(T2)

We write C |> T for a representative of the set of terms T' for which core(T') = T and C(T') = C.

3 . Substitution of of capture sets [x := C1]C2 is defined as follows:

      [x := C1]C2  =  C2\{x} u C1    if x in C2
                   =  C2             otherwise

   Substitution is lifted as a homomorphism to types, with

      [x := C1](C2 |> T) = [x := C1]C2 |> [x := C1]T


4 . As usual, typing rules are of the form `E |- t: T`, where `E` is an environment that binds term and type variables:

    Environment    E  =  {}
                         E, x : T
                         E, X

The `C` function to compute capture sets is extended from types to environments:

         C(E) = U { C(T) | x: T in E }

## Evaluation

Evaluation in the capture calclus is exactly as in call-by-value System F.
A call-by-name version could also be envisaged. It would tend to capture more, due to the non-stict evaluation. Evaluation is formalized below with a single congruence rule that takes
an evaluation context `e`.

__TODO:__ Refine evaluation so that references are tracked at runtime and establish a soundness theorem that dynamically captured references are always a subset of statically predicted captures.

    (beta-v)
                 ((x: T) -> t) v   -->  [x := v]t

    (beta-T)
                    ([X] -> t)[T]  -->  [X := T]t

    (context)
                               t1  -->  t2
                            -----------------
                            e[t1]  -->  e[t2]

where

    Evaluation context e ::= [ ]
                             e t
                             e[T]
                             v e

## Typing Rules

Sub-structural rule:

(weaken)

                              E |- t: T
                            -------------
                            E, E' |- t: T


Syntax directed rules:

(var)

                              x: T in E
                              ---------
                              E |- x: T

(abs)

                            E,x:S |- t: T
                --------------------------------------
                E |- (x: S) -> t : C(E) |> (x: S) -> T

(app)

                    E |- t: C |> (x: C1 |> S) -> T
                 E |- s: C2 |> S    C2 <= [x := C2]C1
                 ------------------------------------
                      `   E |- t s: [x := C2]T

(t-abs)

                             E,X |- t: T
                   --------------------------------
                   E |- [X] -> t : C(E) |> [X] -> T

(t-app)

                        E |- t: C |> [X] -> T
                        ---------------------
                             E |- t[S]: T

Compared to System F, the (abs) and (t-abs) rules augment the result type of the abstracted function with all tracked variables in the current environment. The (weaken) rule ensures that the environment can be reduced to be minimal.

The (app) rule is a bit more complex than the corresponding rule in System F since it has to keep track of capture sets as well as core types. Specifically, it requires that the capture set `C2` of the argument `s`
conforms to the capture set `C1` of the formal parameter `x`, after substituting `x` with `C2`. The substitution provides _capture polymorphism_ by allowing a single capture reference `x` to stand
for an arbitrary set of captures in the argument. The result of the
function application is then the result type of the function, where
`x` is again substituted by `C2`. This resembles function
application for dependent function types except that the dependencies are
restricted to variable tracking. The capture set `C` of the function `t`
itself is discarded in an application.



## Examples

To give some intuition for the calculus and to illustrate capture polymorphism we work out signatures of different versions of the `map` function: `strictMap` maps an arbitrary function argument over a strict list of values. `lazyMap` is like `strictMap` but is defined over lazy lists that get evaluated only on demand. Finally, `pureMap` is like strict map, but it requires that its function argument does not capture anything.

The type signatures presented in this section are quite lengthy.
We will introduce abbreviations that drastically reduce boilerplate afterwards.

If the map functions take the sequence as first argument and the function as second curried argument, their signatures are as follows:

    strictMap:    [A]
               -> [B]
               -> (xs: List[A])
               -> (f: f |> (a: A) -> B)
               -> List[B]

    lazyMap  :    [A]
               -> [B]
               -> (xs: LazyList[A])
               -> (f: f |> (a: A) -> B)
               -> f |> LazyList[B]

    pureMap  :    [A]
               -> [B]
               -> (xs: List[A])
               -> (f: (a: A) -> B)
               -> List[B]

Note the difference between `pureMap` and the first two map functions: `strictMap` and `lazyMap` both declare `f` as a tracked variable, meaning that their function argument can capture arbitrary references. But `pureMap` does not. Hence, its function argument cannot capture tracked variables. The difference between `strictMap` and `lazyMap` is that `lazyMap` captures its function argument `f` in its result since it returns a closure itself. `strictMap` does not.

If the arguments to the map functions are reversed, we get more tracked variables for `strictMap2` and `lazyMap2`, but not for `pureMap2`:

    strictMap2:     [A]
                 -> [B]
                 -> (f: f |> (a: A) -> B)
                 -> f |> (xs: List[A])
                 -> List[B]

    lazyMap2  :     [A]
                 -> [B]
                 -> (f: f |> (a: A) -> B)
                 -> f |> (xs: LazyList[A])
                 -> f |> LazyList[B]

    pureMap2  :     [A]
                 -> [B]
                 -> (f: (a: A) -> B)
                 -> (xs: List[A])
                 -> List[B]

The additional occurrences of `f` in capture sets stem from the fact that now `map f`
is a partial application that captures `f`.

So far, we have assumed that the type of the list argument is simply List[A]. But this is not fully general: `A` could be a type that is tracking variables itself. In this case
none of the map functions could be applied. Indeed, if, say, `A = a |> A1 -> A2` then any
list of `A`s except the empty list would necessarily have type `a |> List[A]`. To see why,
consider the signature of the `cons` function ofor lists that admit impure elements. It must be

    cons[A]: (x: x |> A) -> x |> (xs: xs |> List[A]) -> x |> xs |> List[A]

Therefore, any tracked variable in an element of a list must be also a tracked variable of the list itself. But if `xs` is of type `a |> List[A]` then it does not match the expected type
of any of the map functions above, which is simply `List[A]`. This means that an expression like

    strictMap[A][A](xs)(identity[A][A])

would give a type error. More general and realistic signatures for the map functions would look like this:

    strictMap3:    [A]
                -> [B]
                -> (xs: xs |> List[A])
                -> xs |> (f: f |> (a: A) -> a |> B)
                -> xs |> List[B]

    lazyMap3  :    [A]
                -> [B]
                -> (xs: xs |> LazyList[A])
                -> xs |> (f: f |> (a: A) -> a |> B)
                -> xs |> f |> LazyList[B]

    pureMap3  :    [A]
                -> [B]
                -> (xs |> xs: List[A])
                -> xs |> (f: (a: A) -> a |> B)
                -> xs |> List[B]

These signatures account for the fact that `xs` might track variables itself through its element type `A` and that those tracked variables might appear in the result types of the map functions.

## Shorthand Notations for Borrowing and Capture Polymorphism

The previous examples showed that tracked variables tend to be captured more often than not, at least in code that uses capture polymorphism heavily. This suggests a dual notation that assumes
variables are captured by default and that highlights the _absence_ of capturing instead.

__Variables are captured by default__: We introduce the function type `A -> B` as
   an abbreviation for `(a: A) -> a |> B`. More generally,

    A1 -> A2 ... -> An  ==    (a1: A1)
                           -> a1 |> (a2: A2)
                           -> {a1, a2} |> (a3: A3)
                           -> ...
                           -> {a1, a2, ... a_n-1} |> An

That is, in a curried function type an argument variable is possibly captured by each subsequent result type. However, we do not introduce tracking references if the function
argument is known to be pure. So, for instance

    (A -> B) -> C

would stand for

    (f: (a: A) -> a |> B) -> C

with no capturing introduced for `f`, since we know from `f`'s type `A -> B` that the function cannot capture tracked variables. Note that this is just to reduce notational overhead. One could introduce a capturing reference for `f`, expanding the type instead to

    (f: (a: A) -> a |> B) -> f |> C

As long as `f`'s type is non-capturing, the two signatures are equivalent, in the sense that they admit the same functions that can be applied to the same arguments.

__Absence of capture is made explicit__: Introduce a notation `&A` for "borrowed" parameters. The function type `&A -> B` represents functions where the argument is _not_ captured by the result. I.e. `&A -> B` expands to `(a: A) -> B`. In a curried function type,
borrows are retracted only for the final result type; they are captured in all intermediate
partial application results. That is, assuming all `Ai` except the first are pure:

    &A1 -> A2 -> ... -> An  ==    (a1: A1)
                                -> a1 |> (a2: A2)
                                -> ...
                                -> a1 |> (a_{n-1}: A_{n-1})
                                -> An

__Capture polymorphism is made explicit__: To obtain a smoother notation for capture polymorphism, we introduce the following abbreviation for variable bindings in environments, functions and function types:

    x : (a: A) => B  ==  x : x |> x: (a: A) -> B

That is, the double arrow `=>` represents capture-polymorphic functions. The previous two abbreviations apply to `=>` functions as well. For instance

    &(A => B) -> C -> D

represents the type of functions that take a capture-polymorphic function argument of type `A => B` and a second argument of type `C`. The function's result of type `D` may retain a reference to the tracked variables of `C`, but it does not retain any variables captured by the borrowed first argument. This type expands to the following type that makes all
possible capturing explicit:

    (f: f |> (a: A) -> a |> B) -> f |> (c: C) -> c |> D

__Revised Examples__: Using the shorthand notation, the signatures of the map functions can be written as follows:

    strictMap : [A] -> [B] -> List[A] -> &(A => B) -> List[B]

    lazyMap   : [A] -> [B] -> LazyList[A] -> (A => B) -> LazyList[B]

    pureMap   : [A] -> [B] -> List[A] -> (A -> B) -> List[B]

or, with arguments reversed:

    strictMap2:  [A] -> [B] -> &(A => B) -> List[A] -> List[B]

    lazyMap2  :  [A] -> [B] -> (A => B) -> LazyList[A] -> LazyList[B]

    pureMap2  :  [A] -> [B] -> (A -> B) -> List[A] -> List[B]

This is a lot clearer than the previous signatures! Another advantage is that the new notation is insensitive to the ordering of curried parameters.


## Multiple Signatures

In the capture calculus a function could have several signatures that do not subsume each other. Take again `strictMap`. It declares that any tracked variables in its `List` argument are also captured in its result. That way, its function argument can be fully general. But if we restrict its functon argument to only borrowing functions, the list argument in map becomes borrowed in turn. So an alternative signature for `strictMap` would be:

    strictMap4 : [A] -> [B] -> &List[A] -> &(&A => B) -> List[B]

None of the two versions of `strictMap` subsumes the other. `strictMap4` requires
more from its function argument than `strictMap` and provides more guarantees in turn.
The same erased implementation of `strictMap` can be given either signature.

It's an open question how to accommodate mutliple capture signatures in an actual programming language.

One option would be to do nothing: If two different versions of maps are needed, they need to be
written separately. With overloading resolution one could use the same name for the
various alternatives. In our example, the second `strictMap` is more specific than the first,
since `&A -> B` can be treated as a subtype of `A -> B`.

Another option would be to use multiple signatures for the same body of code, and
to try them all during type checking. To avoid combinatorial explosion, we'd need again some form of overloading resolution to pick a best alternative based on local knowledge.

The third option would be to introduce borrow-polymorphism. If `b&` represents a "borrow" variable, the map function could be described with a single signature:

    strictMap5:  &(b&A => B) -> b&List[A] -> List[B]

But that looks like it could get complex quickly.

## Application To Effect Checking

We describe an application of the capture calculus to effect checking. The principal idea is that instead of modelling effects, we model capabilities. For instance, the effect of throwing an exception can be modeled by a capability `canThrow` that allows to throw an exception. A possible representation of the capability is a closure that when applied to an exception argument throws that argument. Capabilities are simply values that can be passed as arguments to code that needs them. To reduce notational overhead, the value passing can be implicit, using an implicit parameter mechanism such as the one described in [SI](https://infoscience.epfl.ch/record/229878?ln=en). This works all very well, but there is one aspect missing: There is no way to specify that an expression should _not_ have certain capabilities. There are at least two situations where we want to require absence of capabilities:

 - When asserting that a function should be pure, i.e. should have no side effects. In that case we need to ensure that a closure argument does
 not capture any effect capabilities.
 - When introducing effect capabilities with bounded lifetimes. For instance, a `try` block establishes a capability to throw an exception that is in turn caught in the `try` block. Once the `try` terminates, the capability is no longer available. Hence, we need to ensure that the
 capability introduced by a `try` is not captured in its return value.

The capture calculus can be used to check these requirements. To demonstrate this, we augment the capture calculus with a simple effect:
non-local returns. Non-local returns are similar to exceptions. They can be used to emulate exceptions in a straightforward way.
To support non-local returns, the following additions to _CC_ are needed:

Additional syntax:

    Term          s, t  ::=  handle x: T in t
    Eval Context  e     ::=  handle x: T in e

Additional reduction:

(return)

            handle x: T in e[x v]  -->  v

Additional typing rule:

(return)

            E, x: x |> (y: T) -> Unit  |-  t: T    x not in C(T)
            ----------------------------------------------------
                       E |- handle x: T in t: T


The (return) rule introduces a fresh capability `x` and ensures that this capability
is not captured in the handler's result type `T`. From the standpoint of the type system this rule is just syntactic sugar. An alternative would be to model `handle` as a predefined constant that takes a closure as a parameter:

    handle: [T] -> &(T => Unit) -> T


__Example__

To demonstrate non-local returns, here's a small program that sums up the square roots of a list of numbers, returning `NaN` if one of the numbers is negative.

```scala
def root(x: Double)(retrn: Int => Unit) =
  if x < 0 then retrn(Double.NaN)
  sqrt(x)

def sumRoots(xs: List[Double])(retrn: Int => Unit): Double =
  xs match
    case x :: xs1 => root(x) + sumRoots(xs1)
    case Nil => 0

handle retrn: Int in
  sumRoots(List(1.0, 2.0, 3.0, -1.0))(retrn)
```
The program is partitioned into three parts.

 - The `root` function takes the square root of its argument. If the argument is negative
   it signals this fact by invoking the passed `retrn` function with `NaN` as argument.
   `retrn` plays the role of a capability -- wherever it is visible one can use it to
   return to the dynamically enclosing handler that defined `retrn`.

 - The `sumRoots` function sums applies `root` to each element of a list and sums up the results. It passes the `retrn` capability through to `root`. In a language with implicits such as Scala, capabilities would usually be passed as implicit parameters, which keeps
 notational overhead low.

 - The `handle` expression defines the `retrn` capability and passes it on to `sumRoots`.

The program typechecks since it can be shown that the `retrn` value is not captured by the result of `sumRoots(List(1.0, 2.0, 3.0, -1.0))(retrn)`. On the other hand,
the following variation gives a type error:
```scala
handle retrn: Int => Int in
  (x: Int) -> x + sumRoots(List(1.0, 2.0, 3.0, -1.0))(retrn)  // ERROR
```
Here, by rule (abs), `retrn` does appear in the capture set of the handler's body, which
violates the requirement for (return).

As a next step, let's combine non-local returns with the map functions introduced earlier.
We are using the map forms that take the function as first argument.
Here's a slight variation of the previous program. This time, we compute the square roots of a list of `Double` arguments, returning the empty list if any of the arguments are negative.

```scala
val xs: List[Double] = ...
handle retrn: List[Int] in    // retrn: retrn |> List[Int] -> Unit
  val f = strictMap { x =>
    if x < 0 then retrn(Nil)
    sqrt(x)                   // {...}: retrn |> (x: Int) -> Int
  }                           // f: retrn |> List[Int] -> List[Int]
  f(xs)                       // f(xs): List[Int]
```
This program typechecks, with the comments on the right indicating some of the
intermediate types computed.

Here's the same program, using `lazyMap` instead:
```scala
val xs: LazyList[Double] = ...
handle retrn: List[Int] in    // retrn: retrn |> List[Int] -> Unit
  val f = lazyMap { x =>
    if x < 0 then retrn(Nil)
    sqrt(x)                   // {...}: retrn |> (x: Int) -> Int
  }                           // f: retrn |> List[Int] -> retrn |> List[Int]
  f(xs)                       // f(xs): retrn |> List[Int]
                              // ERROR
```
The program fails to typecheck, since `f`'s type indicates that the `retrn`
capability is still captured by its list result. Hence, the contract of `handle`
is violated.


## General Algebraic Effects

As a final case study, we generalize the system of non-local returns to arbitrary algebraic effects. Starting with the basic _CC_ calculus, we add
the following elements:

__Additional Syntax__:

     T  ::=  Handler[S, R, T]

     v  ::=  handler v

     t  ::=  handle x = s in t
             x.suspend t

     e  ::=  handler e
             handle x = e in t
             handle x = v in e
             x.suspend e

There's a new type, `Handler[S, R, T]`, with three type parameters. `S` indicates the type
of values with which an expression can _suspend_. `R` indicates the type of values with which
a suspended computation can _resume_. `T` indicates the final return type of a handled expression.
Handers are values that are created with the `handler` function. There are two other new forms of expressions: The expression `handle x = s in t` establishes the handler `s` under the name `x` in `t`. The expression `x.suspend t` suspends the current computation, passing the argument `t`
to the handler defined by `x`.

__Additional Evaluation Rules__:

     handle x = handler(v)
     in e[x.suspend w]     -->  v w (y -> h -> handle x = h in e[y])

     handle x = handler(v)
     in w                  -->  w

There are two new reduction rules. The first rule connects handlers with suspends. It applies to an expression of the form `handle x = handler(v) in e[x.suspend w]` where the suspension `x.suspend w` appears in evaluation position relative to the handler named `x`. This expression is rewritten to the handler value `v` with two arguments. The first argument is the value `w` passed to the handler from the suspension.
The second argument is a function that takes a resume value `y` and a new handler `h` and
produces the continuation defined by `e` applied to the resume value `y` and wrapped
in the handler `h`. The second reduction discards the handler once its body is evaluated.

__Additional Typing Rules__:

(handler)

            E |- t: S -> (R -> Handler[S, R, T] -> T) -> T
            ----------------------------------------------
                  E |- handler(t): Handler[S, R, T]

(handle)

                        E, s: Handler[S, R, T]
                 E, x: x |> Handler[S, R, T] |- t: T
                           x not in C(T)
                 -----------------------------------
                      E |- handle x = s in t: T

(suspend)

                      E |- x :> Handler[S, R, T]
                     ---------------------------
                        E |- x.suspend: S -> R

The typing rules for algebraic effects are naturally more complex than the ones for
non-local returns, but a core principle stays the same: In both cases the (handle) rule requires that the locally defined handler does not escape in the handled expression's result.


__Example__:

As small but still interesting example of algebraic effects, consider a `counter` handler. When invoked in a suspension, the counter in increments its internal state with the passed value and
resumes the suspension with the new state as argument:
```scala
def counter[T](state: Int) =
  handler[Int, Int, T](incr => k =>
    val newState = state + incr
    k(newState)(counter[T](newState)))
```
Here's an expression that uses the `counter` handler:
```scala
     handle c = counter[Boolean](0)
     in c.suspend(c.suspend(10) + 1) > 20
```
And here's a reduction trace of this expression:
```scala
     handle c = counter[Boolean](0)
     in c.suspend(c.suspend(10) + 1) > 20
 -->
     (incr => k => val newState = 0 + incr; k(newState)(counter(newState)))
     (10) (y => h => handle x = h in c.suspend(y + 1) > 20)
 -->
     (y => h => handle x = h in c.suspend(y + 1) > 20)(10)(counter(10))
 -->
     handle x = counter(10)
     in c.suspend(10 + 1) > 20
 -->
     (incr => k => val newState = 10 + incr; k(newState)(counter(newState)))
     (11) (y => h => handle x = h in y > 20)
 -->
     (y => h => handle x = h in y > 20)(21)(counter(21))
 -->
     handle x = counter(21) in 21 > 20
 -->
     handle x = counter(21) in true
 -->
     true
```

## Relationship With Static Effect Analysis

https://gist.github.com/odersky/9a7f0bddeb73834485fa3c7c71298bab outlines a static effect analysis optimized for functional programs (called _FEA_ in the following) with applications that overlap the ones sketched here.

Compared to _CC_, this effect analysis is a lot more precise. In particular, it allows to reason through data construction and decomposition. Take for instance the canonical functions over pairs:

     pair: [X, Y] -> (x: X) -> (y: Y) -> Pair[X, Y]
     fst:  [X, Y] -> xy: Pair[X, Y] -> X
     snd:  [X, Y] -> xy: Pair[X, Y] -> Y

_FEA_ allows one to conclude that `fst(pair(x, y))` does not reference `y`. _CC_, on the other hand, can only state that `fst` will return some captured part of the pair, which means that
the whole capture set `{x, y}` is returned.

On the other hand, being a whole program static analysis, _FEA_ cannot analyze individual functions separately. It needs to start from some entry points, and it has to make some simplifying
assumptions about what arguments these entry points can take.

There are several interesting avenues for further research in combining some aspects of _CC_ and _FEA_.

 - Use signatures from _CC_ for entry points in _FEA_. _CC_ would be used to give precise meanings to these signatures.
 - Mixed checking: Use _CC_ to capture-check programs. If a type error is detected,
   fall back on _FEA_ to verify that the requirements are met regardless.
 - Similarly, introduce some form of capture casting. This will be needed anyway as an escape hatch. Optionally use _FEA_ to verify that capture casts are correct.
 - Gradual capture typing: Annotate only a subset of methods with capture types.
   Un-annotated methods are checked with _FEA_.
 - Use the richer domains of _FEA_ in a refinement of _CC_. That is, instead of just being sets
   of variables, capture sets might resemble more the abstract values of _FEA_. This would make the type system more heavy-weight. But since capture sets need to be written only rarely,  this might be acceptabe.

_FEA_ remains a very interesting avenue of research by itself, independently of its connections to _CC_. In particular, its added precision for functional programs compared to previous analyses could open some new use cases. Take region allocation as an example: With _CC_, we can check that region allocations don't escape, but the system is relatively coarse. With _FEA_ we could infer in what region a value should be allocated, and do so in a more fine-grained manner.

## Related Work

There's lots, obviously

 - Linear types.
 - Ownership systems with borrowing
 - [Observers for Linear Types](https://link.springer.com/content/pdf/10.1007%2F3-540-55253-7_23.pdf) already goes a bit in the direction outlined here, and can be seen as a precursor of borrowing systems.
 - Tunnelling effects and Effekt. Capture sets are similar to the effect labels in these approaches.
 - Algebraic effects systems
 - Marriage of effects and monads as the current standard approach to model effects
 - Second class values as an alternative approach to prevent captures
 - Our work on implicit abstractions for lightweight capability passing

## To Do List

 - Formulate and prove a type soundness theorem.
 - Work on how to extend _CC_ to a fuller programming language with classes and objects.
   Maybe DOT as a first step.  Would `this` be part of capture sets?
 - Work out how this could be embedded in full Scala.
 - Work on an implementation in Scala's type checker.

