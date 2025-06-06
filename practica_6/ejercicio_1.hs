{-
    Demostrar las siguientes propiedades

    doble = \x -> 2 * x

    Prop.: ¿doble = \x -> 2 * x?
    Dem. : Por el ppio. de ext., es equivalente demostrar que
        ¿ Para todo x. 
            doble x = (\x -> 2 * x) x ?

        Sea n un número cualquiera. Se verá que doble n = (\x -> 2 * x) n

        Lado izq:
                doble n 
            =           (doble)

                2 * n

        Lado der:
                (\x -> 2 * x) n
            =           (beta reducción)
                2 * n
    
    Vale la propiedad.





    compose doble doble = cuadruple

    Prop.: ¿compose doble doble = cuadruple?
    Dem. : Por el ppio. de ext., es equivalente demostrar que
        ¿ Para todo x.
            compose doble doble x = cuadruple x ?

        Sea n un número cualquiera. Se verá que compose doble doble n = cuadruple n

        Lado izq:
                compose doble doble n 
            =                  (compose, f <- doble, g <- doble, x <- n)
                doble (doble n) 
            =                  (doble)
                2 * (doble n)
            =                  (doble)
                2 * (2 * n)
            =                  (aritm)
                4 * n

        Lado der:
                cuadruple n
            =                  (def. cuadruple)
                4 * n

    Vale la propiedad.

-}