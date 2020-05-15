# The Capture Calculus: System F with Capturing

## Abstract Syntax

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
                         handle x: T in t

    Captures       C  =  {x1, ..., xn}
                
    Environment    G  =  {}
                         G, x : T
                         G, X

### Definitions

1. Core types and captured references:

      core(T) = core(T0)     if T = x |> T0
              = T0           otherwise

         C(T) = {x} u C(T0)  if T = x |> T0
                {}           otherwise
            
         C(G) = U { C(T) | x: T in G }

2. Structural equality is alpha renaming +

      T1 === T2   if core(T1) = core(T2) & C(T1) = C(T2)

3. We write C |> T for a representative of the set of terms T' for which core(T') = T and C(T') = C.

4. Substitution [x := C1]C2 is defined as follows:

      [x := C1]C2  =  C2\{x} u C1    if x in C2
                   =  C2             otherwise

   Substitution is lifted as a homomorphism to types, with

      [x := C1](C2 |> T) = [x := C1]C2 |> T


### Evaluation: `t1 -> t2`

(beta-v)
                 ((x: T) -> t) v   -->  [x := v]t

(beta-T)
                    ([X] -> t)[T]  -->  [X := T]t

(return)
            handle x: T in E[x v]  -->  v

(context)
                               t1  -->  t2
                            -----------------
                            E[t1]  -->  E[t2]

where

    Evaluation context E ::= [ ]
                             E t
                             v E
                             handle x: T in E

### Typing Rules

Judgment: `G |- t: T`

Sub-structural rule:

(weaken)

                              G |- t: T
                            -------------
                            G, G' |- t: T


Syntax directed rules:

(var)
                              x: T in G
                              ---------
                              G |- x: T

(abs)
                            G,x:S |- t: T
                --------------------------------------
                G |- (x: S) -> t : C(G) |> (x: S) -> T

                    
(app)
                    G |- t: C |> (x: C1 |> S) -> T
                 G |- s: C2 |> S    C2 <= [x := C2]C1
                 ------------------------------------
                      `   G |- t s: [x := C2]T
                             
(t-abs)
                             G,X |- t: T
                   --------------------------------
                   G |- [X] -> t : C(G) |> [X] -> T
                 
(t-app)
                        G |- t: C |> [X] -> T
                        ---------------------
                             G |- t[S]: T
                             
(handle)
            G,x: x |> (y: T) -> ()  |-  t: T    x notIn C(T)
            ------------------------------------------------
                       G |- hande x: T in t: T


### Abbreviations:

   (x: x |> A -> B) -> C   ==   (x: A => B) -> C
             (x: A) -> B   ==   A -> B     if x notIn C(B)
             (x: A) => B   ==   A => B     if x notIn C(B)

### Examples

1. Various forms of maps

    strictMap: List[A] -> (A => B) -> List[B]

    pureMap  : List[A] -> (A -> B) -> List[B]

    lazyMap  : LazyList[A] -> (f: A => B) -> f |> LazyList[B]



  or, with arguments reversed:

    strictMap:  (f: A => B) -> f |> List[A] -> List[B]

    pureMap  :  (A -> B) -> List[A] -> List[B]

    lazyMap  :  (f: A => B) -> f |> LazyList[A] -> f |> LazyList[B]


  If `A`, `B` can capture things as well, it becomes more complicated:


    strictMap: (xs: List[A]) -> xs |> ((a: A) => a |> B) -> xs |> List[B]

    pureMap  : (xs: List[A]) -> ((x: A) -> a |> B) -> xs |> List[B]

    lazyMap  : (xs: LazyList[A]) -> xs |> (f: (a: A) => a |> B) -> xs,f |> LazyList[B]


2. Full examples, using the second form of maps:

    val xs: List[Double] = ....
    handle retrn: List[Int] in      // retrn: retrn |> List[Int] -> Unit
      val f = strictMap {
        x =>
          if x < 0 then retrn(Nil)
          sqrt(x)                   // {...}: retrn |> (x: Int) -> Int
      }                             // f: retrn |> List[Int] -> List[Int]
      f(xs)                         // f(xs): List[Int]
      
    val xs: LazyList[Double] = ....
    handle retrn: Int in           // retrn: retrn |> List[Int] -> Unit
      val f = lazyMap {
        x =>
          if x < 0 then retrn(0)
          sqrt(x)                   // {...}: retrn |> (x: Int) -> Int
      }                             // f: retrn |> List[Int] -> retrn |> List[Int]
      f(xs)                         // f(xs): retrn |> List[Int]
                                    // ERROR!

### Streamlining the Notation

Instead of listing all captured variables in function signatures,
it might be better to take capturing as the default and use `&A -> B`
as the function type where the argument of `A` is not captured in the
result of type `B`. This would mean we use the following abbreviations:

   A -> B         ==    (a: A) -> a |> B
   &A -> B        ==    (a: A) -> B
   &A -> B -> C   ==    (a: A) -> a |> (b: B) -> b |> C
   &A -> &B -> C  ==    (a: A) -> a |> (b: B) -> C

and so on. A "borrowed" argument `&A` is assumed to be no longer
reachable in the final result part of a (possibly curried) function type.

With streamlined notation, the map operations have the following signatures:

    strictMap: List[A] -> &(A => B) -> List[B]

    pureMap  : List[A] -> (A -> B) -> List[B]

    lazyMap  : LazyList[A] -> (A => B) -> LazyList[B]

or, with arguments reversed:

    strictMap:  &(A => B) -> List[A] -> List[B]

    pureMap  :  (A -> B) -> List[A] -> List[B]

    lazyMap  :  (A => B) -> LazyList[A] -> LazyList[B]

Alternatively, the strict and pure maps could be given signatures that
require and exploit that their function argument does not propagate its arguments:

    strictMap:  &(&A => B) -> &List[A] -> List[B]

    pureMap  :  (&A -> B) -> &List[A] -> List[B]

It's an open question how to represent these different possibilities.

One option would be to do nothing: If two different versions of maps are needed, they need to be
written separately. With overloading resolution one could use the same name for the
various alternatives. In our examples the second `strictMap` is more specific than the first,
since `&A -> B` is treated as a subtype of `A -> B`.

Another option would be to use multiple signatures for the same body of code, and
to try them all. To avoid combinatorial explosion, we'd need again some form
of overloading resolution to pick a best alternative based on local knowledge.

The third option would be to introduce borrow-polymorphism. Then the map functions could be
described with a single signature:


    strictMap:  &(b&A => B) -> b&List[A] -> List[B]

    pureMap  :  (b&A -> B) -> b&List[A] -> List[B]

But that looks like it could get complex quickly.

### Full effect handlers

Additional Syntax:

     T  ::=  ...
             Handler[S, R, T]

     v  ::=  handler v
     
     t  ::=  handle x = s in t
             x.raise t

     E  ::=  ...
             handle x = E in t
             handle x = v in E
             handler E
             x.invoke E

Additional Typing Rules:


            G |- t: S -> (R -> Handler[S, R, T] -> T) -> T
            ----------------------------------------------
                  G |- handler(t): Handler[S, R, T]
                   

                        G, s: Handler[S, R, T]
                 G, x: x |> Handler[S, R, T] |- t: T
                 -----------------------------------
                      G |- handle x = s in t: T

                      G |- x :> Handler[S, R, T]
                     ---------------------------
                        G |- x.invoke: S -> R

Additional Evaluation Rules:

     handle x = handler(v) in E[x.invoke w]  -->  v w (y -> h -> handle x = h in E[y])
                 handle x = handler(v) in w  -->  w

Example:

     def counter[T](state: Int) =
       handler[Int, Int, T](incr => k =>
         val newState = state + incr
         k(newState)(counter[T](newState)))

     handle c = counter[Boolean](0)
     in c.invoke(c.invoke(10) + 1) > 20
 -->
     { incr => k => val newState = 0 + incr; k(newState)(counter(newState)) }
     10 (y => h => handle x = h in c.invoke(y + 1) > 20)
 -->
     (y => h => handle x = h in c.invoke(y + 1) > 20)(10)(counter(10))
 -->
     handle x = counter(10)
     in c.invoke(10 + 1) > 20
 -->
     { incr => k => val newState = 10 + incr; k(newState)(counter(newState)) }
     11 (y => h => handle x = h in y > 20)
 -->
     (y => h => handle x = h in y > 20)(21)(counter(21))
 -->
     handle x = counter(21) in 21 > 20
 -->
     handle x = counter(21) in true
 -->
     true




What is pure?

  A <: Pure
  File {
    def 
  }
  

  