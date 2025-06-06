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
(apply apply) apply :: (a -> b) -> a -> b

-- twice toma una funcion y devuelve una funcion que toma un x
(twice doble) 2 :: Int

((twice twice) twice) swap :: (a, a) -> (a, a)

((flip twice) 1) doble :: Int 
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
-- sin lamba
appDist f (x, y) = (f x, f y)

subst :: (a -> b -> c) -> (a -> b) -> c
subst = \f g x = f x (g x)
-- sin lambda
subst f g x = f x (g x)
------------------------------------------------------------------
-- EJERCICIO 6
{-
    a. compose (fst snd)
    -- asi como está no tipa porque fst recibe una tupla

    compose fst snd :: (a, (b, c)) -> b
    -- si compose toma fst y snd, devuelve una función que tipa

    b. uncurry curry snd :: (a, b) -> b

    uncurry   :: (a -> b -> c) -> (a, b) -> c 
    curry snd :: a -> b -> b
                            --- c <- b
    uncurry curry snd :: (a, b) -> b

    curry   :: ((a, b) -> c) -> a -> b -> c
    snd     :: (a, b) -> b
    ------------------------------- (a, b) <- (a, b)
                        --          c <- b
    curry snd :: a -> b -> b
    
    c. (id apply) apply
      -----------
        apply apply :: (a -> b) -> a -> b

    id          ::
    apply apply ::
    id apply apply ::

    apply :: (a -> b) -> a -> b
    apply :: (a' -> b') -> (a' -> b')
    ---------------------------------- a <- (a' -> b')
                                --     b <- (a' -> b')
    apply apply :: (a' -> b') -> a' -> b'

    d. compose (compose doble doble) :: (a -> Int) -> a -> Int
       
    compose             :: (b -> c) -> (a -> b) -> a -> c 
    compose doble doble :: Int -> Int
    compose (compose doble doble) :: (a -> Int) -> a -> Int

    compose doble :: (a -> Int) -> a -> Int
    doble :: Int -> Int
    compose doble doble :: Int -> Int

    compose       :: (b -> c) -> (a -> b) -> a -> c 
    doble         :: Int -> Int
    compose doble :: (a -> Int) -> a -> Int

    e. no tiene tipado, pero compose (compose doble doble) :: (a -> Int) -> a -> Int

-}
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

------------------------------------------------------------------
-- EJERCICIO 8

-- Curried: es una función que toma una función que recibe un entero
-- y devuelve un entero, y devuelve una función que toma un entero y devuelve un entero
-- Noncurried: es una función toma una funcion y un entero y devuelve un entero
(Int -> Int) -> Int -> Int

-- Curried: es una función que toma una función que recibe a y devuelve una función
-- que toma b y devuelve c, y devuelve una función que toma una función que toma a y devuelve b 
-- y devuelve c
-- Noncurried: toma dos funciones y devuelve un c
(a -> b -> c) -> (a -> b) -> c

-- Curried: es una función que toma un par de funciones y devuelve una funcion que toma
-- un par de elementos y devuelve un par
-- Noncurried: toma un par de funciones y un par de elementos y devuelve un par de elementos
(a -> b, c -> d) -> (a, c) -> (b, d)

-- Curried: es una función que toma una función que toma un par (a, a) y devuelve un b
-- y devuelve una función que toma un a y devuelve b
-- Noncurried: es una función que toma una función de par a b y un elemento a y devuelve b
((a, a) -> b) -> a -> b

(a -> b -> c) -> b -> a -> c

(a -> b) -> (a, a) -> (b, b)

(a -> b, a -> c) -> a -> (b, c)

(a -> b -> c) -> (a -> b) -> a -> c 

a -> b -> a 

------------------------------------------------------------------
-- EJERCICIO 9

-- a. cuadruple x = doble (doble x)
cuadruple x = compose doble doble x
cuadruple x = twice doble x

-- b. timesTwoPlusThree x = suma (doble x) 3
timesTwoPlusThree x = flip suma 3 (doble x)
timesTwoPlusThree x = compose (suma 3) (doble x)
timesTwoPlusThree = compose (suma 3) doble
-- \f g x -> f (g x)
-- \(suma 3) doble x -> suma 3 (doble x)

-- c. fourTimes f x = f (f (f (f x)))
fourTimes f = compose twice twice f
fourTimes = compose twice twice


------------------------------------------------------------------
-- EJERCICIO 10

Notación     | Significado                 | Equivalente en lambda
-------------|-----------------------------|--------------------------
(+3)         | Sumar 3 al argumento        | \x -> x + 3
(3+)         | Sumar argumento a 3         | \x -> 3 + x
(2)          | Multiplicar por 2           | \x -> x 2
(2)          | 2 por argumento             | \x -> 2 x
(/2)         | Dividir por 2               | \x -> x / 2
(2/)         | 2 dividido por argumento    | \x -> 2 / x
(==3)        | ¿Es igual a 3?              | \x -> x == 3
(3==)        | ¿3 es igual a argumento?    | \x -> 3 == x
(>5)         | ¿Mayor que 5?               | \x -> x > 5
(5>)         | ¿5 mayor que argumento?     | \x -> 5 > x



{-
	(==0) . (`mod` 2) . (*2)
	----    ----------------

	compose (==0) (compose (mod 2) (*2))

	\x -> compose (==0) (compose (mod 2) (*2)) x
		  ---------------------------------------
		  									compose con f <- (==0)
														g <- (compose (mod 2) (*2))
														x <- x
	(==0) (compose (mod 2) (*2) x) 
		  -----------------------
									compose con f <- (mod 2)	
												g <- (*2)
												x <- x
	(==0) ((mod 2) ((*2) x))}
		  -----------------
	(==0) 0 == const True
-}
