-- EJERCICIO 7: Esquemas sobre listas

map :: (a -> b) -> [a] -> [b]
map f []        = []
map f (x:xs)    = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter f []     = []
filter f (x:xs) = if f x then x : filter f xs 
                         else filter f xs 

foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr f y []     = y 
foldr f y (x:xs) = f x (foldr f y xs) 

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b 
recr y f []     = y
recr y f (x:xs) = f x xs (recr y f xs)

foldr1 :: (a -> a -> a) -> [a] -> [a]
foldr1 f []     = []
foldr1 f (x:xs) = f x (foldr1 f xs)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith f _ _           = []

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f y []     = b : []
scanr f y (x:xs) = f x y : scanr f y xs

scnr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f y = foldr (\x r = f x (head r) : r) (y : [])

-- EJERCICIO 9

sum :: [Int] -> Int 
sum = foldr (+) 0
-- sum = foldr (\x r -> x + r) 0

length :: a -> Int 
length = foldr (const (+1)) 0
-- length = foldr (\c r -> 1 + r) 0

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []
-- map f = foldr (\x xs -> f x : xs) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x xs -> if f x then x : xs else xs) []

find :: (a -> Bool) -> [a] -> Maybe a
find f = foldr (\x xs -> if f x then Just x else xs) Nothing

any :: (a -> Bool) -> [a] -> Bool
any f = foldr ((||) . f) False
-- any f = foldr (\x xs -> f x || xs) False

all :: (a -> Bool) -> [a] -> Bool 
all f = foldr ((&&) . f) True

countBy :: (a -> Bool) -> [a] -> Int
countBy f = foldr (\x xs -> if f x then 1 + xs else xs) 0

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f = foldr (\x (xs, ys) = if f x then (x : xs, ys) else (xs, x : ys)) ([], [])

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f = foldr (\x xs ys -> case ys of 
                                    [] -> []
                                    (y:ys') -> f x y : xs ys')
                  (const [])

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f y = foldr (\x r = f x (head r) : r) (y : [])

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f = foldr (\x xs -> if f x then x : xs else xs) []

take :: Int -> [a] -> [a]
take = flip (foldr (\x xs n -> if n == 0 then [] else x : xs (n-1)) (const []))

drop :: Int -> [a] -> [a]
drop = flip (foldr (\x xs n -> if n == 0 then x : xs 0 else xs (n-1)) (const []))

elemAt :: Int -> [a] -> a 
elemAt = flip (foldr (\x xs n -> if n == 0 then x else xs (n-1)) (const (error "")))