data Pizza = Prepizza | Capa Ingrediente Pizza
data Ingrediente = Aceitunas Int | Anchoas | Cebolla |
                   Jamon | Queso | Salsa

-- EJERCICIO 1: recursión estructural explícita

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int 
cantidadCapasQueCumplen f Prepizza   = 0
cantidadCapasQueCumplen f (Capa i p) = 
    if f i 
        then 1 + cantidadCapasQueCumplen f p 
        else cantidadCapasQueCumplen f p 

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas f Prepizza   = Prepizza
conCapasTransformadas f (Capa i p) =
    Capa (f i) (conCapasTransformadas f p)

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza 
soloLasCapasQue f Prepizza   = Prepizza
soloLasCapasQue f (Capa i p) =
    if f i 
        then Capa i (soloLasCapasQue f p)
        else soloLasCapasQue f p

cantidadDe :: (Ingrediente -> Bool) -> Pizza -> Int  
cantidadDe f Prepizza = 0 
cantidadDe f (Capa i p) = if fi then 1 + cantidadDe f p else cantidadDe fp


-- EJERCICIO 2
 
sinLactosa :: Pizza -> Pizza 
sinLactosa p = soloLasCapasQue (not esQueso)

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa p = cantidadCapasQueCumplen esQueso p == 0

cantidadDeQueso :: Pizza -> Int 
cantidadDeQueso = cantidadCapasQueCumplen esQueso

conElDobleDeAceitunas :: Pizza -> Pizza 
conElDobleDeAceitunas = conCapasTransformadas dobleAceitunas 

dobleAceitunas :: Ingrediente -> Ingrediente
dobleAceitunase (Aceitunas n) = Aceitunas 2*n 
dobleAceitunas n = n 


-- EJERCICIO 3: Definir

-- expresa la definición de fold para la estructura de Pizza
pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada f x Prepizza   = x  
pizzaProcesada f x (Capa i p) = f i (pizzaProcesada f x p)

-- EJERCICIO 4: utilizar pizzaProcesada

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int 
cantidadCapasQueCumplen g = pizzaProcesada f 0 
    where f i acc = if g i then 1 + acc else acc

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza 
conCapasTransformadas g = pizzaProcesada (\i pp -> Capa (gi) pp) Prepizza

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza 
soloLasCapasQue g = pizzaProcesada f Prepizza 
    where f i pp = if (g i) then Capa i pp else pp 

sinLactosa :: Pizza -> Pizza  
sinLactosa = pizzaProcesada f Prepizza
    where f i pp = if esQueso i 
                        then pp 
                        else Capa i pp

cantidadAceitunas :: Pizza -> Int
cantidadAceitunas = pizzaProcesada f 0 
    where f (Aceitunas n) acc = n + acc 
          f _                 = acc 

capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen g = pizzaProcesada f [] 
    where f i is = if g i 
                    then i : is 
                    else is

capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen f Prepizza = []
capasQueCumplen f (Capa i p) = if f i then 1 + capasQueCumplen f p else capasQueCumplen f p

conDescripcionMejorada :: Pizza -> Pizza 
conDescripcionMejorada = pizzaProcesada f Prepizza
    where f (Aceitunas n) pz = Capa (Aceitunas n * 2) pz 
          f i pz             = Capa i (Capa i pz)

-- agrega las capas de la primera pizza sobre la segunda
conCapasDe :: Pizza -> Pizza -> Pizza 
conCapasDe = pizzaProcesada f (\p -> p)
    where f i pz = \p -> agregarCapa i p 

agregarCapa :: Ingrediente -> Pizza -> Pizza
agregarCapa (Prepizza) pz = pz 
agregarCapa i          pz = Capa i pz 

primerasNCapas :: Int -> Pizza -> Pizza 
primerasNCapas = flip (pizzaProcesada f (\n -> Prepizza))
    where f i h = \n -> if n == 0
                            then Prepizza
                            else agregarCapa i (h (n - 1))
-- TODO: por que flip?

-- EJERCICIO 6: Demostraciones

-- a
{-
    Prop.: ¿para todo f. length . capasQueCumplen f = cantidadDe f?
        Sea g :: (Ingrediente -> Bool), p :: Pizza
    Dem.: por ppio. de induccion sobre la estructura de Pizza, es equivalente demostrar que
        CB. p = Prepizza)
               ¿ length . capasQueCumplen g Prepizza = cantidadDe g Prepizza ?
        CI. p = Capa i pzz)
            HI.1) ¡length . capasQueCumplen g pzz = cantidadDe g pzz!
            TI)  ¿ length . capasQueCumplen g (Capa i pzz) = cantidadDe g (Capa i pzz) ?


    CB)
        Lado der.:
                length . capasQueCumplen g Prepizza
        ->      ----------------------------------- (compose)
                length (capasQueCumplen g Prepizza)
        ->              --------------------------  (def. capasQueCumplen.1)
                length []
        ->      --------
                0

        Lado izq.:
              cantidadDe f Prepizza
        ->    --------------------- (cantidadDe.1)
                0
    
        Vale la propiedad.

    CI)
        Lado der.:
            length . capasQueCumplen g (Capa i pzz)
        ->                                          (.)
            length (capasQueCumplen g (Capa i pzz))
        ->          ------------------------------  (capasQueCumplen.2)
            length (if g i then 1 + capasQueCumplen g pzz else capasQueCumplen g pzz)

            Por analisis de casos
                Caso 1. if g i == True)
                         1 + capasQueCumplen g pzz
                
                Caso 2. if g i == False)
                        capasQueCumplen g pzz

        Lado izq.:
            cantidadDe g (Capa i pzz)
        ->  ------------------------- (cantidadDe.2)
            if g i then 1 + cantidadDe g pzz else cantidadDe g pzz

            Por analisis de casos:
                Caso 1. if g i == True)
                    1 + cantidadDe g pzz
                
                Caso 2. if g i == False)
                    cantidadDe g pzz
-}  

