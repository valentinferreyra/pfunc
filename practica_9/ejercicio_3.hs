data Tree a = EmptyT
            | NodeT a (Tree a) (Tree a)

-- describe el número resultante de sumar todos los números
-- en el árbol dado
sumarT :: Tree Int -> Int  
sumarT EmptyT          = 0
sumarT (NodeT n li ld) = n + sumarT li + sumarT ld

-- describe la cantidad de elementos en el árbol dado
sizeT :: Tree a -> Int 
sizeT EmptyT          = 0
sizeT (NodeT _ li ld) = 1 + sizeT li + sizeT ld 

-- indica si en el arbol dado hay al menos un elemento
-- que cumple con el predicado dado
anyT :: (a -> Bool) -> Tree a -> Bool
anyT _ EmptyT             = False
anyT f (NodeT n li ld)    = f n || anyT f li || anyT f ld 

-- describe la cantidad de elementos en el arbol que cumplen
-- con el predicado dado
countT :: (a -> Bool) -> Tree a -> Int
countT _ EmptyT             = 0
countT f (NodeT n li ld)    = if f n 
                                then 1 + countT f li + countT f ld 
                                else countT f li + countT f ld

-- describe la cantidad de hojas en el árbol dado
countLeaves :: Tree a -> Int 
countLeaves EmptyT          = 0
countLeaves (NodeT _ li ld) = 1 + countLeaves li + countLeaves ld

-- describe la altura del árbol
heightT :: Tree a -> Int 
heightT EmptyT          = 0
heightT (NodeT _ li ld) = 1 + max (heightT li) (heightT ld)

-- describe la lista in order con los elementos del arbol dado
inOrder :: Tree a -> [a]
inOrder EmptyT          = []
inOrder (NodeT n li ld) = inOrder li ++ [n] ++ inOrder ld 

-- describe la lista donde cada elemento es una lista con los elementos
-- de un nivel del árbol
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT n li ld) = [n] : (mergeLists (listPerLevel li) (listPerLevel ld))

mergeLists :: [[a]] -> [[a]] -> [[a]]
mergeLists xss []            = xss 
mergeLists [] yss            = yss 
mergeLists (xs:xss) (ys:yss) = (xs ++ ys) : (mergeLists xss yss)

-- describe un arbol con los mismos elementos que el arbol dado
-- pero en orden inverso
mirrorT :: Tree a -> Tree a
mirrorT EmptyT            = EmptyT
mirrorT (Node T n li ld)  = Node T n (mirrorT ld) (mirrorT li)

-- describe la lista con los elementos del nivel dado en el arbol dado
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT             = []
levelN 0 (Node x li ld)     = [x]
levelN n (NodeT x li ld)    = levelN (n-1) li ++ levelN (n-1) ld

-- describe la lista con los elementos de la rama mas larga del arbol
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x li ld) = x : if heightT li > heightT ld 
                                    then ramaMasLarga li 
                                    else ramaMasLarga ld

-- describe la lista con todos los caminos existentes en el arbol dado
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x li ld) = (addToEveryList x todosLosCaminos li) ++ (addToEveryList x todosLosCaminos ld)

addToEveryList :: a -> [[a]] -> [[a]]
addToEveryList x []       = []
addToEveryList x (xs:xss) = (x : xs) ++ addToEveryList x xss

{-
Demostrar

    i. Prop.: ¿heightT = length . ramaMasLarga?
       Dem. : por ppio. de extensionalidad, es equivalente demostrar:
            para todo t :: Tree a,
                ¿heightT t = (length . ramaMasLarga) t?
            por definicion de compose, es equivalente demostrar:
                ¿heightT t = length (ramaMasLarga t)?

        Por ppio de induccion sobre la estructyura de t, es equivalente demostrar que:

            Sea a :: Tree a.

        CB. a = EmptyT)
                ¿heightT EmptyT = length (ramaMasLarga EmptyT)?
        
        CI. a = NodeT x li ld)
                HI.1) ¡heightT li = length (ramaMasLarga li)!
                HI.2) ¡heightT ld = length (ramaMasLarga ld)!
                TI)   ¿heightT (NodeT x li ld) = length (ramaMasLarga (NodeT x li ld))?
        
        Demo. caso base:
            Lado izq.:
                    heightT EmptyT
                =   -------------           (heightT.1)
                    0
            
            Lado der.:
                    length (ramaMasLarga EmptyT)
                =           -------------------  (ramaMasLarga.1)
                    length []
                =   --------                     (length, x <- [])
                    0
        Vale la propiedad.

        Dem. caso inductivo:
            Lado izq.:
                    heightT (NodeT x li ld)
                =   ---------------------- (heightT.2)
                    1 + max (heightT li) (heightT ld)

            Lado der.:
                    length (ramaMasLarga (NodeT x li ld))
                =   Lema 1
                    1 + max (length (ramaMasLarga li)) (length (ramaMasLarga ld))
                = H1, H2
                    1 + max (heightT li) (heightT ld)
        
        Vale la propiedad

            Lema 1: 
                Prop.: ¿length (ramaMasLarga (NodeT x li ld)) = 1 + max (length (ramaMasLarga li)) (length (ramaMasLarga ld))?
                Dem.: 
                    Demostracion por analisis de casos:
                
                    Caso a) length (ramaMasLarga li) > (length (ramaMasLarga ld)
                        Lado izq.:
                                length (ramaMasLarga (NodeT x li ld))
                            =           ---------------------------- (ramaMasLarga.2)
                                length (x : ramaMasLarga li)
                            =   
                                1 + (length (ramaMasLarga li))
                        
                        Lado der.:
                                1 + max (length (ramaMasLarga li)) (length (ramaMasLarga ld))
                            =       ---------------------------------------------------------
                                1 + (length (ramaMasLarga li))

                Se cumple para este caso. Trivial para el ld.
-}

-- ----------------------------------------------------------
{-
    Prop.: ¿reverse . inOrder = inOrder . mirrorT?
    Dem. : por ppio. de ext. es equivalente demostrar que.:
        Para todo t : Tree a, 
            ¿(reverse . inOrder) t = (inOrder . mirrorT) t?
        Por definicion de compose, es equivalente demostrar que:
          Para todo t : Tree a, 
            ¿reverse (inOrder t) = inOrder (mirrorT t)?
        Sea n :: Tree a, por ppio. de induccion es equivalente demostrar.:

        CB. n = EmptyT)
                ¿reverse (inOrder EmptyT) = inOrder (mirrorT EmptyT)?
        CI. n = NodeT x li ld)
                HI.1) ¡reverse (inOrder li) = inOrder (mirrorT li)!
                HI.2) ¡reverse (inOrder ld) = inOrder (mirrorT ld)!
                TI) ¿reverse (inOrder (NodeT x li ld)) = inOrder (mirrorT (NodeT x li ld))?

        Dem. caso base)
            Lado izq.:
                    reverse (inOrder EmptyT)
                =            -------------- (inOrder.1)
                    reverse []
                =   --------- (reverse.1)
                    []
            
            Lado der.:
                    inOrder (mirrorT EmptyT)
                =   ----------------------- (mirrorT.1)
                    inOrder EmptyT
                =   ------------- (inOrder.1)
                    []
        Vale la propiedad.


        Dem. caso inductivo)
            Lado izq.:
                    reverse (inOrder (NodeT x li ld))
                =           ------------------------ (inOrder.2)
                    reverse (inOrder li ++ [x] ++ inOrder ld)
                = Lema 2
                    reverse (inOrder ld) ++ reverse [x] ++ reverse (inOrder li)
                = HI.1 HI.2
                    inOrder (mirrorT ld) ++ [x] ++ inOrder (mirrorT li)

            Lado der.:
                    inOrder (mirrorT (NodeT x li ld))
                =           ------------------------- (mirrorT.2)
                    inOrder (NodeT x (mirrorT ld) (mirrorT li))
                =   ----------------------------------------- (inOrder.2)
                    inOrder (mirrorT ld) ++ [x] ++ inOrder (mirrorT li)
            
        Vale la propiedad.

        
        Dem. Lema 2)
            Sean xs :: [a], ys :: [a], zs :: [a], 
                ¿reverse (xs ++ ys ++ zs) = reverse xs ++ reverse ys ++ reverse zs?

            

-}