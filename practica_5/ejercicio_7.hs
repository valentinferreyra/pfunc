-- PRACTICA 7

data Set a = S (a -> Bool) 

belongs :: Set a -> a -> Bool
-- que dado un conjunto, describe la función que indica si un elemento dado pertenece a ese conjunto.
belongs (S f) x = f x 

-- ejemplo 
numerosMayoresA10 :: Set Int 
numerosMayoresA10 = S (\n -> n > 10)

empty :: Set a 
-- describe el conjunto vacío
empty = S (\n -> False)

singleton :: Eq a => a -> Set a
-- dado un elemento describe un conjunto que contiene a ese único elemento
singleton x = S (\n -> n == x)

union :: Set a -> Set a -> Set a 
-- dados dos conjuntos, describe al conjunto que resulta de la unión de ambos
union s1 s2 = S (\n -> belongs s1 n || belongs s2 n)

intersection :: Set a -> Set a -> Set a 
--dados dos conjuntos, describe al conjunto que resulta de la intersección de ambos
intersection s1 s2 = S (\n -> belongs s1 n && belongs s2 n)
