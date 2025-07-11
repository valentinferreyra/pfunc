data QuadTree a = LeafQ a 
            |     NodeQ (QuadTree a) (QuadTree a) 
                        (QuadTree a) (QuadTree a)

data Color = RGB Int Int Int 
type Image = QuadTree Color 

-- describe la altura del arbol dado
heightQT :: QuadTree a -> Int 
heightQT (LeafQ _)           = 1
heightQT (NodeQ l1 l2 l3 l4) = 
        1 + max (heightQT l1) (max (heightQT l2) (max (heightQT l3) (heightQT l4)))

-- describe la cantidad de hojas del arbol dado
countLeavesQT :: QuadTree a -> Int 
countLeavesQT (LeafQ _)           = 1
countLeavesQT (NodeQ l1 l2 l3 l4) =
    countLeavesQT l1 + countLeavesQT l2 + countLeavesQT l3 + countLeavesQT l4

-- describe la cantidad de constructores
sizeQT :: QuadTree a -> Int 
sizeQT (LeafQ _)           = 1
sizeQT (NodeQ l1 l2 l3 l4) =
    1 + sizeQT l1 + sizeQT l2 + sizeQT l3 + sizeQT l4

-- describe el arbol resultante de transformar en hoja todos aquellos
-- nodos para los que se cumplan que todas sus hojas tienen el mismo valor
compress :: QuadTree a -> QuadTree a 
compress (LeafQ l)           = LeafQ l 
compress (NodeQ l1 l2 l3 l4) =
    compress' (compress l1) (compress l2) (compress l3) (compress l4)

compress' :: QuadTree a -> QuadTree a -> QuadTree a -> QuadTree a -> QuadTree a
compress' (LeafQ l1) (LeafQ l2) (LeafQ l3) (LeafQ l4) =
    juntarSiIguales l1 l2 l3 l4
compress' qt1 qt2 qt3 qt4 = NodeQ qt1 qt2 qt3 qt4

juntarSiIguales :: a -> a -> a -> a -> QuadTree a 
juntarSiIguales x1 x2 x3 x4 = 
    if x1 == x2 && x3 == x4 & x1 == x4 
        then LeafQ x1 
        else NodeQ (LeafQ x1) (LeafQ x2) (LeafQ x3) (LeafQ x4)

-- describe el arbol resultante de transformar en nodo (manteniendo el dato)
-- todas aquellas hojas que no se encuentren en el nivel de la altura del arbol
uncompress :: QuadTree a -> QuadTree a 