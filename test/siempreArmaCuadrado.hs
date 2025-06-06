data Shape = Circle Float | Rect Float Float
data Gusto = Chocolate | DDL | Frutilla | Sambayon 
data Helado = Vasito Gusto 
            | Cucurucho Gusto Gusto
            | Pote Gusto Gusto Gusto 

esCuadrado :: Shape -> Bool
esCuadrado (Rect n1 n2) = n1 == n2
esCuadrado _            = False 
 
siempreArmaCuadradoDeTamano :: Float -> (Float -> Shape) -> Bool 
siempreArmaCuadradoDeTamano n f = esCuadradoDeTamano n (f n)

esCuadradoDeTamano :: Float -> Shape -> Bool 
esCuadradoDeTamano n (Rect n1 n2) = n1 == n2 && n1 == n
esCuadradoDeTamano n _            = False 

siempreArmaCuadrado :: (Float -> Shape) -> Bool 
siempreArmaCuadrado f = esCuadrado (f 1) 

cuadrado :: Float -> Shape 
cuadrado n = Rect n n 

noCuadrado :: Float -> Shape 
noCuadrado n = Rect n (n+1)

shapeNormal :: (Float -> a) -> a 
shapeNormal c = c 1

-- siempreArmaCuadrado cuadrado :: True
-- siempreArmaCuadrado noCuadrado :: False
-- siempreArmaCuadrado (Rect 2) :: False   
-- siempreArmaCuadrado (shapeNormal Rect) :: True

----------------------------------------------------------

data Par a = MkP a a 
chocoHelate consH = consH Chocolate 

-- Mk True False :: Par Bool
-- MkP (Vasito DDL) (chocoHelate Vasito) :: Par Helado
-- MkP (chocoHelate Cucurucho) Vasito :: Par (Gusto -> Helado)

-- MkP (cuadrado DDL) :: no tipa porque cuadrado DDL no tiene tipo
-- y para poder generar un Par a, si o si necesito construirlo en base a un tipo a


