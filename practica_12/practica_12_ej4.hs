-- EJERCICIO 4

type Record a b = [(a, b)]

type Table a b = [Record a b]

-- a partir de la lsita de registros dada describe la lista de
-- registros que cumplen con la condición dada
select :: (Record a b -> Bool) -> Table a b -> Table a b 
select f []     =
select f (r:rs) =