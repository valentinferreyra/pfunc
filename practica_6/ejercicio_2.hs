{-
    Demostar

    para todo x. para todo y. 
        x && y = not ((not x) || (not y))

    Dem. : ¿ para todo x. para todo y. 
        x && y = not ((not x) || (not y)) ?

    
        Sean a y b booleanos. Se verá que a && b = not ((not a) || (not b))

        Se deben analizar diferentes casos

        Caso a. True y b = True)

        Lado izq:
                a && b
            =           (def &&)
                True

        Lado der:
                not ((not a) || (not b))
            =           (def not)
                not (False || False)
            =           (def ||)
                not False
            =           (def not)
                True

    Vale este caso.

    Caso b. a = False)

            Lado izq:
                False && b
            =   
                False

            Lado der:
                not ((not False) || (not b))
            =   
                not (True || (not b))
            =           (def ||)
                not (True)
            =
                False

    Vale este caso.


    Caso c. b = False)
     Lado izq:
                a && False
            =           (def &&)
                False

        Lado der:
                not ((not a) || (not False))
            =           (def not)
                not ((not a) || True)
            =           (def ||)
                not True
            =           (def not)
                False

    Vale este caso.
    Vale la propiedad.
-}