-- EJERCICIO 1

data Gusto = Chocholate | DulceDeLeche | Frutilla | Sambayon 

data Helado = Vasito Gusto 
            | Cucurucho Gusto Gusto
            | Pote Gusto Gusto Gusto 

chocoHelate consH = consH Chocolate 

-- Determinar el tipo de las siguientes expresiones
Vasito :: Gusto -> Helado 
Chocolate :: Gusto 
Cucurucho :: Gusto -> Gusto -> Helado 
Sambayon :: Gusto 
Pote :: Gusto -> Gusto -> Gusto -> Helado 
chocoHelate :: (Gusto -> Helado) -> Helado
chocoHelate Vasito :: Helado 
chocoHelate Cucurucho :: Gusto -> Helado 
chocoHelate (Cucurucho Sambayon) :: Helado -- Cucurucho Chocolate Sambayon

chocoHelate (chocoHelate Cucurucho) :: Helado
{-
chocoHelate (chocoHelate Cucurucho)
------------------------------------
->                        chocoHelate con consH <- (chocoHelate Cucurucho)
(chocoHelate Cucurucho) Chocolate
 ---------------------
->                        chocoHelate con consH <- Cucurucho

Cucurucho Chocolate Chocolate
--------------------------------
Helado

(Cucurucho Chocolate) es una funcion que toma un gusto y devuelve un helado
-}

chocoHelate (Vasito DulceDeLeche) -- no tipa
chocoHelate Pote :: Gusto -> Gusto -> Helado 
chocoHelate (chocoHelate (Pote Frutilla)) :: Helado

{-
chocoHelate (chocoHelate (Pote Frutilla))
             ---------------------------
chocoHelate (Pote Chocolate Frutilla)
--------------------------------------
Pote Chocolate Chocolate Frutilla
-}

