data Shape = Circle Float | Rect Float Float

esCuadrado :: Shape -> Bool
esCuadrado (Rect n1 n2) = n1 == n2
esCuadrado _            = False 
 
siempreArmaCuadradoDeTamano :: Float -> (Float -> Shape) -> Bool 
siempreArmaCuadradoDeTamano n f = esCuadrado (f n) 
                               && esCuadradoDeTamano n (f n)

esCuadradoDeTamano :: Float -> Shape -> Bool 
-- PRECOND: es un cuadrado
esCuadradoDeTamano n (Rect n1 n2) = n1 == n
esCuadradoDeTamano n _            = False 

siempreArmaCuadrado :: (Float -> Shape) -> Bool 
siempreArmaCuadrado f = siempreArmaCuadradoDeTamano 1 f 

armadorDeCuadrados :: Float -> Shape 
armadorDeCuadrados n = Rect n n 

noArmadorDeCuadrados :: Float -> Shape 
noArmadorDeCuadrados n = Rect n (n+1)