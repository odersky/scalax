# The Capture Calculus: System F with Capturing

## Abstract Syntax

    Variable       x, y, z
    Type variable  X, Y, Z

    Type           T  =  X                   type variable
                         (x: T)T             function
                         [X]T                type function
                         {x}T                T capturing x

    class Key {
      type T
      val x: T
    }
    
    val k: Key = Key { type T = Int, val x: this.T = 0 }
    

    val y: k.T = k.x

    Value          v  =  (x: T) -> t
                         [X] -> t
    Term           t  =  v
                         x
                         t t
                         t[T]
                         handle x: T in t

    Captures       C  =  {x1, ..., xn}
                
    Environment    G  =  {}
                         G, x : T
                         G, X

Note: `/` binds less than `->`: (x: T) -> U / C  =  ((x: T) -> U) / C

Note: Substitution [x := C1]C2 is defined as follows:

      [x := C1]C2  =  C2\{x} u C1    if x in C2
                   =  C2             otherwise

Substitution is lifted as a homomorphism to types, with

      [x := C1](C2 |> T) = [x := C1]C2 |> T


Definions - Core types and captured references:

      core(T) = core(T0)     if T = x |> T0
              = T0           otherwise

         C(T) = {x} u C(T0)  if T = x |> T0
                {}           otherwise
            
         C(G) = U { C(T) | x: T in G }

### Structural equality

Alpha renaming +

      T1 = T2   if core(T1) = core(T2) & C(T1) = C(T2)

We write C |> T for a representative of the set of terms T' for which core(T') = T and C(T') = C.


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

## Typing Rules


### Typing  `G |- t: T`

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
                         G |- t s: [x := C2]T
                             
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

    strictMap: List[A] -> (A => B) -> List[B]

    pureMap  : List[A] -> (A -> B) -> List[B]

    lazyMap  : LazyList[A] -> (f: A => B) -> f |> LazyList[B]

or, with arguments reversed:

    strictMap(f): List[A] -> List[B]

    strictMap:  (f: A => B) -> f |> List[A] -> List[B]

    lazyMap  :  (f: A => B) -> f |> LazyList[A] -> f |> LazyList[B]

    pureMap  :  (A -> B) -> List[A] -> List[B]


    handle retrn: Int in
      ...
      if x == 1 then retrn 3
      ...
      4

   try
     if == 1 then throw Ret(3)
     
   catch
     case Ret(n) => n
     


### Full effect handlers


     t  ::=  handle x: T, R = s in t
             resume t1 t2
             abort t

     G, resume: T -> S -> R  |-  s: S -> R    G, x: S -> T |- t: R
     ------------------------------------------------------------------
     G |- handle x: S, T, R = s in t: R
     

     E  ::=  handle s, r = E in t
             handle x = v in E

               handle s = v in E[s w]  -->  handling x = v w in E[x w]
               handle s = resume s, e

              
               
                handling x = v in E[x w]  -->  v
                handle s = 
           handling x =  v in E[x w]  -->  v

               handle s, r = v in


let f(x: Int) = resume
handle counter =
  def loop(x: Int) = resume x, () => loop(x + 1)
  () => loop(0)
in


