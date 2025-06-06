{-
    Demostrar

    curry suma' = suma

    Prop.: ¿curry suma' = suma?
    Dem. : Por el ppio. de ext. (aplicado dos veces), es equivalente demostar que
        ¿ Para todo x. para todo y.
            curry suma' x y = suma x y ?

        Sea n y m dos números, se verá que curry suma' n m = suma n m

        Lado izq.:
                curry suma' n m
            =               (curry, f <- suma', x <- n, y <- m)
                suma' (n, m)
            =               (suma')
                n + m

        Lado der.:
                suma n m
            =               (suma, x <- n, y <- m)
                n + m

    Vale la propiedad.


    uncurry suma = suma'

    Prop.: ¿uncurry suma = suma'?
    Dem. : Por el ppio. de ext., es equivalente demostar que
        ¿ Para todo (x, y). 
            uncurry suma (x, y) = suma' (x, y) ?

        Sea (n, m) un par de números, se verá que
            uncurry suma (n, m) = suma' (n, m)

        Lado izq.:
                uncurry suma (n, m)
            =               (uncurry, f <- suma, x <- (n, m))
                suma n m
            =               (def. suma)
                n + m

        Lado der.:
                suma' (n, m)
            =           (suma')
                n + m
    
    Vale la propiedad.
-}      