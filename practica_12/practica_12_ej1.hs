-- EJERCICO 1
data ExpA = Cte Int 
        |   Suma ExpA ExpA
        |   Prod ExpA ExpA

foldExpA :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b 
foldExpA c s p (Cte n) = c n 
foldExpA c s p (Suma n1 n2) = s (foldExpA c s p n1) (foldExpA c s p n2)
foldExpA c s p (Prod n1 n2) = p (foldExpA c s p n1) (foldExpA c s p n2)

cantidadDeCeros :: ExpA -> Int 
cantidadDeCeros = foldExpA unoSiCero (+) (+)
    where unoSiCero :: Int -> Int 
          unoSiCero 0 = 1 
          unoSiCero _ = 0

noTieneNegativosExplicitosExpA :: ExpA -> Bool 
noTieneNegativosExplicitosExpA = foldExpA
                                    esNegativo
                                    (&&)
                                    (&&)
            where esNegativo :: Int -> Bool 
                  esNegativo n = n < 0

-- describe una expresion con el mismo significado, pero que no tiene
-- sumas del numero 0 ni multiplicaciones por 1 o por 0
simplificarExpA' :: ExpA -> ExpA
simplificarExpA' = foldExpA 
                        Cte 
                        simplificarSumaExpA
                        simplificarProdExpA

simplificarSumaExpA :: ExpA -> ExpA -> ExpA
simplificarSumaExpA (Cte 0) exp2 = exp2
simplificarSumaExpA exp1 (Cte 0) = exp1
simplificarSumaExpA exp1 exp2    = Suma exp1 exp2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Cte 1) exp2 = exp2
simplificarProd exp1 (Cte 1) = exp1
simplificarProd (Cte 0) exp2 = Cte 0
simplificarProd exp1 (Cte 0) = Cte 0
simplificarProd exp1 exp2 = Prod exp1 exp2

evalExpA' :: ExpA -> Int 
evalExpA' = foldExpA id (+) (*)

showExpA :: ExpA -> String 
showExpA = foldExpA (\n -> "(Cte" ++ show e ++ ")")
                    (\n1 n2 -> "(Suma" ++ n1 ++ n2 ++ ")")
                    (\n1 n2 -> "(Prod" ++ n1 ++ n2 ++ ")")

-- dar el tipo y definir recExpA, que expresa el esquema de recursión primitiva
-- para la estructura ExpA
recExpA :: (Int -> b) 
            -> (b -> b -> ExpA -> ExpA -> b) 
            -> (b -> b -> ExpA -> ExpA -> b) 
            -> ExpA 
            -> b
recExpA c s p (Cte n) = c n 
recExpA c s p (Suma n1 n2) = s (recExpA c s p n1) (recExpA c s p n2) n1 n2 
recExpA c s p (Prod n1 n2) = p (recExpA c s p n2) (recExpA c s p n2) n1 n2 

-- describe la cantidad de constructores de suma con al menos uno de sus hijos
-- constante cero
cantDeSumaCeros :: ExpA -> Int 
cantDeSumaCeros = foldExpA (const 0)
                           (sumExpABy (anyCteN 0))
                           (\n1 n2 _ _ -> n1 + n2)

sumExpABy :: (ExpA -> ExpA -> Bool) -> Int -> Int -> ExpA -> ExpA -> Int
sumExpABy f n1 n2 exp1 exp2 = if f exp1 exp2 then 1 + n1 + n2 else n1 + n2

anyCteN :: Int -> ExpA -> ExpA -> Bool 
anyCteN x (Cte n1) (Cte n2) = x == n1 || x == n2  
anyCteN x (Cte n) _         = x == n 
anyCteN x _ (Cte n)         = x == n
anyCteN x _ _               = False 