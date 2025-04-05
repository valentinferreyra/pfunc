-- EJERCICIO 1
-- Indicar los tipos de las siguientes funciones

-- first (x, y) = x
first :: (a, b) -> a

-- apply f = g where g x = f x
apply :: (a -> b) -> a -> b

-- twice f = g where g x = f (f x)
twice :: (a -> a) -> a -> a

-- doble x = x + x
doble :: Int -> Int 

-- swap (x, y) = (y, x)
swap :: (a, b) -> (b, a)

-- uflip f = g where g p = f (swap p)
uflip :: ((b, a) -> c) -> (a, b) -> c


---------------------------------------------------------
-- EJERCICIO 2
-- Dadas las definiciones anteriores, indicar el tipo de las siguientes                   
-- expresiones

-- apply first
apply first :: (a, b) -> a

-- first (swap, uflip)
first (swap, uflip) :: (a, b) -> (b, a)

-- twice doble
twice doble :: Int -> Int 

-- twice twice
-- a <- (a' -> a')
twice twice :: (a -> a) -> a -> a 

-- twice uflip
twice :: (a -> a) -> (a -> a)
uflip :: ((b, a) -> c) -> (a, b) -> c
-------------------------------------------- -- a <- (a, a) , a <- c
twice uflip :: ((a, a) -> c) -> ((a, a) -> c)
\f -> \p -> twice uflip f p 

-- twice swap
twice :: (a -> a) -> a -> a
swap  :: (a, b) -> (b, a)
twice swap :: (a, a) -> (a, a)

-- uflip swap
uflip ::  ((b, a) -> c) -> (a, b) -> c
swap :: (a, b) -> (b, a)
------------------------------------ -- (b, a) <- (a, b)
                                     -- c <- (b, a)
uflip swap :: (b, a) -> (b, a)

-- (twice twice) swap
(twice twice) swapp :: (a, a) -> (a, a)


---------------------------------------------------------
-- EJERCICIO 3
const :: a -> (b -> a)
const x = g
    where g y = x

appDupp :: ((a, a) -> b) -> (a -> b)
appDup f = g
    where g x = f (x, x)

appFork :: (a -> b, a -> c) -> (a -> (b, c)) 
appFork (f, g) = h
    where h x = (f x, g, x)

appPar :: (a -> b, c -> d) -> ((a, c) -> (b, d))
appPar (f, g) = h 
    where h (x, y) = (f x, g y)

appDist :: (a -> b) -> ((a, a) -> (b, b))
appDist f = g
    where g (x, y) = (f x, f y)

flip :: (a -> (b -> c)) -> (b -> (a -> c))
flip f = h
    where h x = k
        where k y = (f y) x

subst :: (a -> (b -> c)) -> ((a -> b) -> (a -> c))
subst f = h
    where h g = k
        where k x = (f x) (g x)


---------------------------------------------------------
-- EJERCICIO 4

-- a) 1 && 2 == 2 No
-- b) 1 + if 3 < 5 then 3 else 5 :: Int
-- c) let par = (True, 4) in (if first par then first par else second par) :: No tiene
-- d) (doble doble) 5 No tiene
-- e) doble (doble 5) :: Int
-- f) twice first :: No tiene
-- g) (twice doble) doble :: No tiene
-- h) (twice twice) first No
-- i) apply apply :: (a -> b) -> a -> b

---------------------------------------------------------
-- EJERCICIO 6

appFork :: (a -> b, a -> c) -> (a -> (b, c)) 
\p -> let (f, g) = p
    in \x -> (fx, gx)

subst :: (a -> (b -> c)) -> (a -> b) -> (a -> c)  
\f -> (\g -> (\x -> f x (g x))

flip :: (a -> (b -> c)) -> (b -> (a -> c))
\f -> (\x -> (\y -> (f y) x))

appDist :: (a -> b) -> ((a, a) -> (b, b)) 
\f -> (\px -> let (x, y) = px in (f x, f y))

const :: a -> (b -> a)
\x -> (\y -> x)

appPar :: (a -> b, c -> d) -> ((a, c) -> (b, d))
\pf -> let (f, g) = pf in \px -> let (x, y) = px in (f x, gy)

appDup :: ((a, a) -> b) -> (a -> b)
\f -> (\x > f (x, x))

---------------------------------------------------------
-- EJERCICIO 7

appFork (id, id)
----------------        appFork con f <- id, g <- id
\x -> (id x, id x)  --  appFork (id, id) = h where h x = (id x, id x) 
      ------------      id con x <- x
\x -> (x, x)        --  definicion de id

-- appFork (id, id) es una función que dado un x retorna un par (x, x)
appFork (id, id) :: a -> (a, a)

-- !--------------------------------------------------------- --

\f -> appDup (appDist f)
      ------------------        appDup con f <- appDist f
\f -> \x -> appDist f (x, x)
            ----------------    appDist con f <- f y (x, y) <- (x, x)
\f -> \x -> (f x, f x)

-- \f -> appDup (appDist f) es una función que dado una función f y un x, devuelve un par
-- donde aplica f a cada elemento
\f -> appDup (appDist f) :: ((a -> b) -> (a -> (b, b)))

-- !--------------------------------------------------------- --

appDup id
---------               appDup con f <- id
\x -> id (x, x)
      --------          id con x <- (x, x)
\x -> (x, x)

-- appDup id es una función que dado un x retorna un par (x, x)
appDup id :: a -> (a, a)

-- appDup id es equivalente a appFork (id, id)

-- !--------------------------------------------------------- --

appDup appFork 
--------------              appDup con f <- appFork
\x -> appFork (x, x)
      -------------         appFork con f <- x y g <- x
\x -> \y -> (x y, x y)  --  beta reducción

-- appDup appFork: dame una función x y un y, te devuelvo un par donde cada elemento es 
-- y aplicado en x

-- appDup appFork es equivalente a \f -> appDup (appDist f)

-- !--------------------------------------------------------- --

flip (appDup const)
     -------------          appDup con f <- const
\x -> flip (const (x, x))
      -------------------   flip con f <- const (x, x)
\x -> \y -> const (x, x) y
            --------------  const con x <- (x, x)
\x -> \y -> (x, x)

-- flip (appDup const):: dame un x y un y y te devuelvo el par (x, x)
flip (appDup const) :: (a -> (b -> (a, a)))

-- !--------------------------------------------------------- --

const (appDup id)
       ---------        appDup con f <- id
\x -> const (id (x, x))
\x -> const (x, x)
\x -> \y -> (x, x)

-- const (appDup id) :: dame un x y un y te devuelvo el par (x, x)
const (appDup id) :: (a -> (b -> (a, a)))

-- const (appDup id) es equivalente a flip (appDup const)
