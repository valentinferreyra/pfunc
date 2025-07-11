data Arbol a = Hoja a | Nodo a (Arbol a) (Arbol a)

foldT :: (a -> b -> b -> b) -> (a -> b) -> Arbol a -> b 
recrT :: (a -> b) -> (a -> Arbol a -> b -> Arbol a -> b -> b) -> Arbol a -> b

data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA
foldExpA :: (Int -> b) -> (b -> b -> b) (b -> b -> b) -> ExpA -> b
recExpA :: (Int -> b) -> (b -> ExpA -> b -> ExpA -> b) -> (b -> ExpA -> b -> ExpA -> b) -> ExpA -> b

data EA = Const Int | BOp BinOp EA EA 
data BinOp = Sum | Mul

foldEA :: (Int -> a) -> (BinOp -> b -> b -> b) -> EA -> b 
recEA :: (Int -> a) -> (BinOp -> b -> EA -> b -> EA -> b) -> EA -> b

Data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
foldTree :: b -> (a -> b -> b -> b) -> Tree a -> b
foldTree z cr EmptyT          = z 
foldTree z cr (NodeT x t1 t2) = cr x (foldTree z cr t1) (foldTree z cr t2)

recTree :: b -> (a -> b -> Tree a -> b -> Tree a -> b) -> Tree a -> b
recTree z cr EmptyT          = z
recTree z cr (NodeT x t1 t2) = cr x t1 (recTree z cr t1) t2 (recTree z cr t2)

mapT :: (a -> b) -> Tree a -> Tree b 
mapT f = foldTree EmptyT (\x r1 r2 -> NodeT (f x) r1 r2) 

sumT :: Tree Int -> Int
sumT = foldTree 0 (\x n1 n2 -> x + n1 + n2)

sizeT :: Tree a -> Int 
sizeT = foldTree 0 (\x n1 n2 -> 1 + n1 + n2)

heightT :: Tree a -> Int 
heightT = foldTree 0 -> (\x n1 n2 -> 1 + max n1 n2)

preOrder :: Tree a -> [a]
preOrder = foldTree [] (\x xs1 xs2 -> x : (xs1 ++ xs2))

inOrder :: Tree a -> [a]
inOrder = foldTree [] (\x xs1 xs2 -> xs1 ++ (x : xs2))

mirrorT :: Tree a -> Tree a 
mirrorT = foldTree EmptyT (\x r1 r2 -> NodeT x r2 r1)

countByT :: (a -> Bool) -> Tree a -> Int 
countByT f = foldTree 0 (\x n1 n2 -> if f x 
                                        then 1 + n1 + n2 
                                        else n1 + n2)

zipWithT :: (a -> b - > c) -> Tree a -> Tree b -> Tree c
zipWithT f = foldTree (const EmptyT) (\x r1 r2 -> \tb -> case t of 
                                            EmptyT -> EmptyT
                                            NodeT y t1 t2 -> NodeT (f x y) (r1 t1) (r2 t2))   

insertT :: a -> Tree a -> Tree a
interT e = recTree (NodeT e EmptyT EmptyT)
                   (\x t1 r1 t2 r2 -> if e == x
                    then NodeT x t1 t2
                    else if e > x 
                        then NodeT x t1 r2
                        else NodeT x r1 t2)
