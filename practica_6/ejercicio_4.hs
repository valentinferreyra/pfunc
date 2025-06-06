{-
    Demostrar

    curry fst = const

    Prop.: ¿curry fst = const?
    Dem. : Por el ppio. de ext. (dos veces), es equivalente demostrar que
        ¿ Para todo x. para todo y.   
            curry fst x y = const x y ?

    Sean f y g dos elementos cualquiera.
        Se verá que curry fst f g = const f g

    
    Lado izq.: 
            curry fst f g
        =               (curry, f <- fst, x <- f, y <- g)
            fst (f, g)
        =               (fst)
            f

    Lado der.:
            const f g
        =               (const, x <- f, y <- g)
            f
        
    Vale la propiedad.


    uncurry (flip const) = snd

    Prop.: ¿uncurry (flip const) = snd?
    Dem. : Por el ppio. de ext. es equivalente demostar que
        ¿ Para todo (x, y).
            uncurry (flip const) (x, y) = snd (x, y) ?

        Sea (f, g) un par de elementos cualquiera.
        Se verá que uncurry (flip const) (f, g) = snd (f, g)

        Lado izq.: 
                uncurry (flip const) (f, g)
            =               (uncurry, f <- flip const, x <- (f,g))
                flip const f g
            =               (flip, f <- const, x <- f, y <- g)
                const g f
            =               (const)
                g
        
        Lado der.:
                snd (f, g)
            =           (snd)
                g

    Vale la propiedad.

-}

twice :: (a -> a) -> a -> a
twice f x = f (f x)

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

compose12 :: (d -> c) -> (a -> b -> d) -> a -> b -> c 
compose12 f g x y = f (g x y)