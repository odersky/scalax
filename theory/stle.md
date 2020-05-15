# Simply Typed Lambda Calculus with Effects

## Abstract Syntax

    Variable       x, y, z
    Type variable  X, Y, Z

    Value Type     V = B                            base type
                       X                            type variable
                       T -> T                       pure function
                       T => T                       impure function
    Type           T = V
                       E                            effect capability
    Value          v = lambda(x: T)t
                       x
    Term           t = v
                       t t
    Environment    G = x1: T1, ..., xn: Tn          (xi != xj)

***

### Evaluation: `t -> t'`

    (beta-V)
                  (lambda(x: T)t) v  -->  [x := v]t

## Typing Rules

### Purity `T pure`, `G pure`

                                B pure

                                X pure

                                E pure

                          T pure    T' pure
                          -----------------
                            (T -> T') pure

                         V1: pure ... Vn pure
                       ------------------------
                       x1: V1, ..., xn: Vn pure


### Type Assignment `G |- t: T`

    (Weaken)
                              G |- t: T
                            -------------
                            G, G' |- t: T
    (Narrow)
                              G |- t: T
                      -------------------------
                      [X := V]G |- t: [X := V]T
    (Var)
                              x: T in G
                              ---------
                              G |- x: T
    (Sub)
                           G |- t: T -> T'
                           ---------------
                           G |- t: T => T'
    (Abs-V-impure)
                           G, x: V |- t: T
                      --------------------------
                      G |- lambda(x: V)t: V => T
    (Abs-V-pure)
                      G pure    G, x: V |- t: T
                      --------------------------
                      G |- lambda(x: V)t: V -> T
    (Abs-E)
                           G, x: E |- t: T
                      --------------------------
                      G |- lambda(x: E)t: E -> T
    (App)
                    G |- t1: T => T'    G |- t2: T
                    ------------------------------
                            G |- t1 t2: T'

#System-FE

    Value Type      V = ...
                        All(X)V
    Value           v = ...
                        Lambda(X)t
    Term            t = ...
                        t[V]

### Purity `T pure`: ...

                                V pure
                             ------------
                             All(X)V pure


### Type Assignment `G |- t: T`

                     G |- t: V    X not in tv(G)
                     ---------------------------
                      G |- Lambda(X)t: All(X)V

                           G |- t: All(X)V'
                        ----------------------
                        G |- t[V]: [X := V']V'

Soundness Property:

Assume:

    t: T -> T'
    v: T

    t v -->* v'

Then none of the reductions is of the form

    (lambda(x: E)t)y --> [x := y]t

where `y` is free in `t v`.



