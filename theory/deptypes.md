Alphabets
  A_n   (n >= 0)   
  x_n ranges over A_n
  x ranges over Union(A_i)

Term  t_i  ::=  x_i
                (x_j : t_{j+1}) -> t_i
                forall t_i
                t_i t_j
                U_i            if i >= 2

We let x range over variables of arbitrary universes
We let s, t, S, T range over terms of arbitary universes.
When used together S, T are understood to be in a higer universe than s, t, x.
We use `_` as a wildcard standing for a term in an arbitrary universe that is unused elsewhere.

Common abbreviations:

       U_2 = Type
       U_3 = Kind

### Typing rules


                              x: T in G
                              ----------
                              G |- x : T

                    G |- S : _    G, x : S |- t: T
                   --------------------------------------
                   G |- (x: S) -> t  :  forall(x: S) -> T

               G |- t : forall(x: S) -> T    G |- s : S
               ----------------------------------------
                         G |- t s : [x := s]T

                              G |- t: T
                           ----------------
                           G |- forall t: T

                          G |- U_n : U_{n+1}


### Simple 2 Level System

Term  t    ::=  x
                (x : T) -> t
                [X] -> t
                t t
                t [T]
                
Type T     ::=  X
                (x: T) -> T
                [X] -> T
