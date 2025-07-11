-- EJERCICIO 2
data EA = Const Int | BOp BinOp EA EA 
data BinOp = Sum | Mul 

foldEA :: (Int -> b) -> (BinOp -> b -> b -> b) -> EA -> b 
foldEA c b (Const n)         = c n 
foldEA c b (BOp bop ea1 ea2) = b bop (foldEA c b ea1) (foldEA c b ea2)

noTieneNegativosExplicitosEA :: EA -> Bool 
noTieneNegativosExplicitosEA = foldEA (>0) 
                                      (\_ res1 res2 -> res1 && res2)

simplificarEA' :: EA -> EA 
simplificarEA' = foldEA Const simplificarOP 

simplificarOP :: BinOp -> EA -> EA -> EA
simplificarOP Sum e1 e2 = case e1 of
					(Const 0) -> e2
					_ -> case e2 of
					           (Const 0) -> e1
							_ -> (BOp Sum e1 e2)
simplificarOP Mul e1 e2 = case e2 of
					 (Const 1) -> e1
					 (Const 0) -> Const 0
					 _ -> case e1 of
							(Const 1) -> e2
							(Const 0) -> Const 0
							_ -> (BOp Mul e1 e2)

evalEA' :: EA -> Int 
evalEA' = foldEA id (\bop res1 res2 -> case bop of 
                                        Suma -> res1 + res2 
                                        Mul -> res1 * res2)

ea2ExpA' :: EA -> ExpA
ea2ExpA' = foldEA Cte (\bop res1 res2 -> case bop of 
                                        Suma -> Suma res1 res2 
                                        Mul -> Prod res1 res2)

ea2Arbol' :: EA -> ABTree BinOp Int 
ea2Arbol' = foldEA Leaf Node 

