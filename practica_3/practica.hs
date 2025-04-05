-- práctica currificación

-- EJERCICIO 1
curry :: ((a, b) -> c) -> a -> b -> c 
curry = \f x y -> f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry = \f (x, y) -> f x y

------------------------------------------------------------------
-- EJERCICIO 2 y 3
apply :: (a -> b) -> a -> b
apply f x = f x

twice :: (a -> a) -> a -> a
twice f x = f (f x)

id :: a -> a
id x = x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

uflip :: ((b, a) -> c) -> (a, b) -> c  
uflip f p = f (swap p)

const :: a -> b -> a 
const x y = x

compose :: (b -> c) -> (a -> b) -> a -> c 
compose f g x = f (g x)

------------------------------------------------------------------
-- EJERCICIO 4
{-

-- apply toma una función y un x y hace f x
apply apply apply :: (a -> b) -> a -> b

-- twice toma una funcion y devuelve una funcion que toma un x
(twice doble) 2 :: Int

twice twice twice swap

flip twice 1 doble
-}

------------------------------------------------------------------
-- EJERCICIO 5

appDup :: ((a, a) -> b) -> a -> b
appDup = \f x -> f (x, x)
-- sin lambda
appDup f x = f (x, x)

appFork :: (a -> b, a -> c) -> a -> (b, c)
appFork = \(f, g) x = (f x, g x)
-- sin lamba
appFork (f, g) x = (f x, g x)

appPar :: (a -> c, b -> d) -> (a, b) -> (c, d)
appPar = \(f, g) (x, y) = (f x, g y)
-- sin lambda
appPar (f, g) (x, y) = (f x, g y)

appDist :: (a -> b) -> (a, a) -> (b, b)
appDist = \f (x, y) = (f x, f y)
-- sin lambra
appDist f (x, y) = (f x, f y)

subst :: (a -> b -> c) -> (a -> b) -> c
subst = \f g x = f x (g x)
-- sin lambda
subst f g x = f x (g x)

------------------------------------------------------------------
-- EJERCICIO 7

many :: Int -> (a -> a) -> a -> a 
many 0 f x = x
many n f x = f (many (n-1) f x)

-- con id y compose
many 0 f x = id x
many n f x = compose f (many (n-1) f) x

many 0 f = id  
many 0 f = compose f (many (n-1) f) 

