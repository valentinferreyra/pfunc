-- EJERCICIO 5

data Shape = Circle Float | Rect Float Float

construyeShNormal :: (Float -> Shape) -> Shape
construyeShNormal c = c 1.0

-- Determinar el tipo

uncurry Rect :: (Float, Float) -> Shape 

construyeShNormal (flip Rect 5.0) :: Shape

{-
construyeShNormal (flip Rect 5.0)
                   ------------- 
->                                  flip con f <- Rect
construyeShNormal (g 5.0)           where g x = k where k y = (Rect y) x
                  -------
->                                  g con x <- 5.0

construyeShNormal k                 where k y = (Rect y) 5.0
-------------------
->                                  def. de construyeShNormal
k 1.0
-----
->                                  beta redux
Rect (1.0) 5.0 :: Shape
-}

compose (uncurry Rect) swap :: (Float, Float) -> Shape

{-
    compose (uncurry Rect) :: (a -> (Float, Float)) -> a -> Shape
    swap :: (a, b) -> (b, a)
    -----------------------------
    compose (uncurry Rect) swap :: (Float, Float) -> Shape



    compose :: (b -> c) -> (a -> b) -> a -> c 
    uncurry Rect :: (Float, Float) -> Shape 
    -----------------------------
    compose (uncurry Rect) :: (a -> (Float, Float)) -> a -> Shape
-}

uncurry Cucurucho :: (Gusto, Gusto) -> Helado

uncurry Rect swap no tipa

{-
    uncurry Rect :: (Float, Float) -> Shape 
    swap :: (a, b) -> (b, a)
    -----------------------
    uncurry Rect swap :: x
-}

compose uncurry Pote :: Gusto -> (Gusto, Gusto) -> Helado

{-
    compose uncurry :: (a -> b -> c -> d) -> a -> (b, c) -> d
    Pote :: Gusto -> Gusto -> Gusto -> Helado
    -----------------------
    compose uncurry Pote :: Gusto -> (Gusto, Gusto) -> Helado


    compose :: (b -> c) -> (a -> b) -> a -> c 
    uncurry :: (a -> b -> c) -> (a, b) -> c
    ----------------------
    compose uncurry :: (a -> b -> c -> d) -> a -> (b, c) -> d
-}

compose Just :: (a -> b) -> a -> Maybe b

{-
    compose :: (b -> c) -> (a -> b) -> a -> c 
    Just :: a -> Maybe a
    -------------------- b <- a; c <- Maybe a
    compose Just :: (a -> a) -> a -> Maybe a
    pero el a de compose y el de Just son distintos
        compose Just :: (a -> b) -> a -> Maybe b
-}

compose uncurry (Pote Chocolate) :: X

{-
    compose uncurry :: (a -> b -> c -> d) -> a -> (b, c) -> d
    Pote Chocolate :: Gusto -> Gusto -> Helado
    -----------------------------------
    compose uncurry (Pote Chocolate) :: x no  tipa
-}

-- Funciones complementarias
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry = \f (x, y) -> f x y

flip :: (a -> (b -> c)) -> (b -> (a -> c))
flip f x y = f y x

compose :: (b -> c) -> (a -> b) -> a -> c 
compose f g x = f (g x)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon 

data Helado = Vasito Gusto 
            | Cucurucho Gusto Gusto
            | Pote Gusto Gusto Gusto 

data Maybe a = Nothing | Yast a
