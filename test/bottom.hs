const :: a -> b -> a
const a b = a 

bottom :: a
bottom = bottom 

-- const 2 bottom = 2
-- const bottom 2 = bottom

-- plt: haskell utiliza orden de reducción normal
	-- empieza por el mas externo
	-- no ejecuta lo que no necesita
	-- permite esquivar el bottom

compose :: (b -> c) -> (a -> b) -> a -> c 
compose f g x = f (g x)

doble :: Int -> Int
doble x = x + x

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