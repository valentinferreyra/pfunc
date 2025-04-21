-- EJERCICIO 8

data MayFail a = Raise Exception | Ok a 
    deriving Show

data Exception = DivByZero | NotFound | NullPointer
            | Other String
    deriving Show

type ExHandler a = Exception -> a 

tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b
-- dada una computación que puede fallar, una función que indica cómo continuar si
-- no falla, y un manejador de los casos de falla, expresa la computación completa
tryCatch (Ok    a) f eh = f a
tryCatch (Raise e) f eh = eh e

-- ejemplo
dividir :: Int -> Int -> MayFail Int 
dividir x y = if y == 0 then Raise DivByZero 
                        else Ok (div x y)

intentarDividir :: Int -> Int -> String
intentarDividir x y = 
        tryCatch (dividir x y) siEsCeroAvisar exMsg 

siEsCeroAvisar :: Int -> String 
siEsCeroAvisar x = if x == 0 then "Es cero" else "No es cero"

exMsg :: Exception -> String 
exMsg DivByZero = error "No se puede dividir por 0"
exMsg _         = error "chumbazo por pelotudo"