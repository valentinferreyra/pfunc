data Arbol a b = Hoja b 
            |    Nodo a (Arbol a b) 
                        (Arbol a b)

-- describe la cantidad de hojas en el árbol dado
cantidadDeHojas :: Arbol a b -> Int 
cantidadDeHojas (Hoja _)       = 1
cantidadDeHojas (Nodo _ li ld) = cantidadDeHojas li + cantidadDeHojas ld 

-- describe la cantidad de nodos en el árbol dado
cantidadDeNodos :: Arbol a b -> Int
cantidadDeNodos (Hoja _)        = 0
cantidadDeNodos (Nodo _ li ld)  = 1 + cantidadDeNodos li + cantidadDeNodos ld

-- describe la cantidad de constructores en el árbol dado
cantidadDeConstructores :: Arbol a b -> Int 
cantidadDeConstructores (Hoja _)       = 1
cantidadDeConstructores (Nodo _ li ld) = 1 + cantidadDeConstructores li + cantidadDeConstructores ld 

-- describe la representación como elemento del tipo Arbol BinOp Int
-- de la expresion aritmetica dada
ea2Arbol :: EA -> Arbol BinOp Int 
ea2Arbol (Const n)          = Hoja n  
ea2Arbol (BOp bnop e1 e2)   =
             Nodo bnop (ea2Arbol e1) (ea2Arbol e2)


data EA = Const Int | BOp BinOp EA EA
data BinOp = Sum | Mul

{-
    Prop.: ¿cantidadDeHojas t + cantidadDeNodos t = cantidadDeConstructores t?
    Dem. : por ppio de induccion sobre la estructura de t, es equivalente demostrar que

    Sea n :: Arbol a b

    CB. n = Hoja x) 
        ¿cantidadDeHojas (Hoja x) + cantidadDeNodos (Hoja x) = cantidadDeConstructores (Hoja x)?

    CI. n = Nodo x li ld)
        HI.1) ¡cantidadDeHojas li + cantidadDeNodos li = cantidadDeConstructores li!
        HI.2) ¡cantidadDeHojas ld + cantidadDeNodos ld = cantidadDeConstructores ld!
        TI)   ¿cantidadDeHojas (Nodo x li ld) + cantidadDeNodos (Nodo x li ld) = cantidadDeConstructores (Nodo x li ld)?

    Dem. caso base)
        Lado izq.:
                cantidadDeHojas (Hoja x) + cantidadDeNodos (Hoja x)
            =   ------------------------                            (cantidadDeHojas.1)
                    1  + cantidadDeNodos (Hoja x)
            =            -----------------------                    (cantidadDeNodos.1)
                    1 + 0
            = aritm
                    1
        
        Lado der.:
                cantidadDeConstructores (Hoja x)
            =   -------------------------------     (cantidadDeConstructores.1)
                1
            
        Vale la propiedad.

    
    Dem. caso inductivo)
        Lado izq.:
                cantidadDeHojas (Nodo x li ld) + cantidadDeNodos (Nodo x li ld)
            =   ------------------------------         (cantidadDeHojas.2)
                cantidadDeHojas li + cantidadDeHojas ld + cantidadDeNodos (Nodo x li ld)
            =                                             ------------------------------ (cantidadDeNodos.2)
                cantidadDeHojas li + cantidadDeHojas ld + 1 + cantidadDeNodos li + cantidadDeNodos ld
            = conmutatividad y asociatividad en la suma
                1 + (cantidadDeHojas li + cantidadDeNodos li) + (cantidadDeHojas ld + cantidadDeNodos ld)
            = H1, H2
                1 + cantidadDeConstructores li + cantidadDeConstructores ld
        

        Lado der.:
                cantidadDeConstructores (Nodo x li ld)
            =   ------------------------------------- (cantidadDeConstructores.2)
                1 + cantidadDeConstructores li + cantidadDeConstructores ld

            Vale la propiedad.
-}      
