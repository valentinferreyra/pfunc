-- PRACTICA 3
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

foldT :: b -> (a -> b -> b -> b) -> Tree a -> b 
foldT cb cr EmptyT = cb 
foldT cb cr (NodeT x t1 t2) = cr x (foldT cb cr t1) (foldT cb cr t2)

mapT :: (a -> b) -> Tree a -> Tree b 
mapT f = foldT EmptyT 
               (NodeT . f)

sumT :: Tree Int -> Int 
sumT = foldT 0 (\n r1 r2 -> n + r1 + r2)
