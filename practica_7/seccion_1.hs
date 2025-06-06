-- EJERCICIO 1

data Pizza = Prepizza | Capa Ingrediente Pizza
data Ingrediente = Aceitunas Int | Anchoas | Cebolla |
                   Jamon | Queso | Salsa

{-
 REGLAS:
    Regla base: Prepizza está en Pizza
    Regla inductiva: Si p está en Pizza 
                     entonces Capa Ingrediente p está en Pizza

Forma esquemática:

f :: Pizza -> T
f Prepizza   = ...
f (Capa i p) = ... f p ... 
-}

cantidadDeCapas :: Pizza -> Int 
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p

cantidadDeAceitunas :: Pizza -> Int 
cantidadDeAceitunas Prepizza   = 0
cantidadDeAceitunas (Capa i p) = cantidadDeAceitunas' i +
                                 cantidadDeAceitunas p

cantidadDeAceitunas' :: Ingrediente -> Int
cantidadDeAceitunas' (Aceitunas n) = n
cantidadDeAceitunas' _ = 0 

duplicarAceitunas :: Pizza -> Pizza 
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) = Capa (duplicarAceituna i) (duplicarAceitunas p)

duplicarAceituna :: Ingrediente -> Ingrediente
duplicarAceituna (Aceitunas n) = Aceitunas 2*n
duplicarAceituna i = i

sinLactosa :: Pizza -> Pizza 
sinLactosa Prepizza = Prepizza
sinLactosa (Capa i p) = if esLactosa i 
                            then sinLactosa p
                            else Capa i (sinLactosa p) 

esLactosa :: Ingrediente -> Bool 
esLactosa Queso = True 
esLactosa _     = False 

aptaIntolerantesLactosa :: Pizza -> Bool 
aptaIntolerantesLactosa Prepizza   = True 
aptaIntolerantesLactosa (Capa i p) = not (esLactosa i) && aptaIntolerantesLactosa p

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza = Prepizza
conDescripcionMejorada (Capa i p) = unirAceitunas i (conDescripcionMejorada p)

unirAceitunas :: Ingrediente -> Pizza -> Pizza
unirAceitunas (Aceitunas n1) (Capa (Aceitunas n2) p) = Capa (Aceitunas n1+n2) p 
unirAceitunas i              c                       = Capa i c

-- EJERCICIO 4
-- Demostrar

{-
[A]
    Prop.: ¿cantidadDeAceitunas Prepizza = cantidadDeAceitunas (conDescripcionMejorada Prepizza)?
    Dem. : 

    Lado izq.:
            cantidadDeAceitunas Prepizza
    ->                                      (def. cantidadDeAceitunas)
            0

    Lado der.:
            cantidadDeAceitunas (conDescripcionMejorada Prepizza)
    ->                                     (def. conDescripcionMejorada)
            cantidadDeAceitunas Prepizza
    ->                                     (def. cantidadDeAceitunas)
            0

[B]

    Prop.: ¿cantidadDeAceitunas (Capa Queso Prepizza) = cantidadDeAceitunas (conDescripcionMejorada (Capa Queso Prepizza))
    Dem.:

    Lado izq.:
            cantidadDeAceitunas (Capa Queso Prepizza)
        ->                               (def. cantidadDeAceitunas)
            cantidadDeAceitunas' (Capa Queso Prepizza)
        -> 
            0

    Lado der.:
            cantidadDeAceitunas (conDescripcionMejorada (Capa Queso Prepizza))
        ->  
           cantidadDeAceitunas (Capa Queso Prepizza)
        ->
           cantidadDeAceitunas' (Capa Queso Prepizza)
        ->
            0

[C]
    Prop.: ¿cantidadDeAceitunas (Capa (Aceitunas 8) 
                          (Capa Queso Prepizza))
            =
             cantidadDeAceitunas 
                (conDescripcionMejorada  
                        (Capa (Aceitunas 8) 
                            (Capa Queso Prepizza)))?
    Dem.:

    Lado izq.:
            cantidadDeAceitunas (Capa (Aceitunas 8) 
                          (Capa Queso Prepizza))
        ->
            cantidadDeAceitunas' (Capa (Aceitunas 8)) + cantidadDeAceitunas (Capa Queso Prepizza)
        ->
            8 + cantidadDeAceitunas' (Capa Queso Prepizza)
        -> 
            8 + 0
        -> 
            8

    Lado der.:
        cantidadDeAceitunas 
                (conDescripcionMejorada  
                        (Capa (Aceitunas 8) 
                            (Capa Queso Prepizza)))
    -> 
        cantidadDeAceitunas 
                        (Capa (Aceitunas 8) 
                            (Capa Queso Prepizza))
    -> 
        cantidadDeAceitunas (8 + 0)
    ->
        8

[D]
    Prop.: ¿cantidadDeAceitunas (Capa (Aceitunas 9)  
                          (Capa (Aceitunas 8) 
                                (Capa Queso Prepizza)))
            = 
            cantidadDeAceitunas 
                (conDescripcionMejorada  
                    (Capa (Aceitunas 9)  
                        (Capa (Aceitunas 8) 
                            (Capa Queso Prepizza))))?

    Dem.:

    Lado izq.:
            cantidadDeAceitunas (Capa (Aceitunas 9)  
                                    (Capa (Aceitunas 8) 
                                        (Capa Queso Prepizza)))
    ->
        cantidadDeAceitunas' (Capa (Aceitunas 9) +
            cantidadDeAceitunas (Capa (Aceitunas 8) 
                                        (Capa Queso Prepizza))
    -> 
        9 + cantidadDeAceitunas' (Capa (Aceitunas 8) + cantidadDeAceitunas' (Capa Queso Prepizza)
    ->
        9 + 8 + 0
    -> 
        17

    Lado der.:
            cantidadDeAceitunas 
                (conDescripcionMejorada  
                    (Capa (Aceitunas 9)  
                        (Capa (Aceitunas 8) 
                            (Capa Queso Prepizza))))
    ->
            cantidadDeAceitunas
                    (Capa (Aceitunas 17)
                            (Capa Queso Prepizza))
    ->
        cantidadDeAceitunas'  (Capa (Aceitunas 17)) 
            + cantidadDeAceitunas' (Capa Queso Prepizza)
    ->
        17 + 0
    ->
        17
-}  