data EA = Const Int | BOp BinOp EA EA
data BinOp = Sum | Mul

-- Describe el número que resulta de evaluar la cuenta
-- representada por la expresión aritmética dada
evalEA :: EA -> Int 
evalEA (Const n)         = n
evalEA (BOp bop ea1 ea2) = evalBinOp bop (evalEA ea1) (evalEA ea2)

evalBinOp :: BinOp -> Int -> Int -> Int 
evalBinOp Sum = (+)
evalBinOp Mul = (*)

data ExpA = Cte Int 
        |   Suma ExpA ExpA
        |   Prod ExpA ExpA

-- describe una expresión aritmética representada con el tipo
-- ExpA, cuya estructura y significado son los mismos que la dada
ea2ExpA :: EA -> ExpA
ea2ExpA (Const n)         = Cte n
ea2ExpA (BOp bop ea1 ea2) = binOp2expAOp bop (ea2ExpA ea1) (ea2ExpA ea2)

binOp2expAOp :: BinOp -> ExpA -> ExpA -> ExpA 
binOp2expAOp Sum exp1 exp2 = Suma exp1 exp2  
binOp2expAOp Mul exp1 exp2 = Prod exp1 exp2

-- describe una expresión aritmética representada con el tipo EA,
-- cuya estructura y significado son los mismos que la dada.
expA2ea :: ExpA -> EA 
expA2ea (Cte n)          = Const n
expA2ea (Suma exp1 exp2) = BOp Sum (expA2ea exp1) (expA2ea exp2)
expA2ea (Prod exp1 exp2) = BOp Mul (expA2ea exp1) (expA2ea exp2)


-- Demostrar

{-
    i. Prop.: ¿ea2ExpA . expA2ea = id?
       Dem. : 
        Por ppio. de extensionalidad, es equivalente demostrar que:
            para todo e :: ExpA. ¿(ea2ExpA . expA2ea) e = id e?
        Por definición de compose, es equivalente demostrar que:
            para todo e :: ExpA. ¿ea2ExpA (expA2ea e) = id e?

        Sea n :: ExpA. Por ppio de inducción en la estructura de n,

        CB1. n = Cte n')
            Lado izq. 
                    ea2ExpA (expA2ea (Cte n'))
            =               ----------------- (expA2ea.1)
                    ea2ExpA (Const n')
            =       -----------------         (ea2ExpA.1)
                    Cte n'

            Lado der.
                    id (Cte n')
            =       -----------               (id, x <- Cte n')
                    Cte n'
        Vale la propiedad.


        CI1. n = Suma n1 n2)
            HI1) ¡ea2ExpA (expA2ea n1) = id n1!
            HI2) ¡ea2ExpA (expA2ea n2) = id n2!
            TI)  ¿ea2ExpA (expA2ea (Suma n1 n2)) = id (Suma n1 n2)?

        Dem.:
            Lado izq.
                 ea2ExpA (expA2ea (Suma n1 n2))
            =             --------------------  (expA2ea.2)
                 ea2ExpA (BOp Sum (expA2ea n1) (expA2ea n2)) 
                 -------------------------------------------
            =                                    (ea2ExpA.2)
                binOp2expAOp Sum (ea2ExpA (expA2ea n1)) (ea2ExpA (expA2ea n2))
            =   --------------------------------------------------------------     
                                                 (binOp2expAOp.1)
                Suma (ea2ExpA (expA2ea n1)) (ea2ExpA (expA2ea n2))
            =        ----------------------      (HI1)
                Suma (id n1) (ea2ExpA (expA2ea n2))
            =                ----------------------
                                                 (HI2)
                Suma (id n1) (id n2)
            =        ------  -------             (id)
                Suma n1 n2

            Lado der.
                 id (Suma n1 n2)  
            =    ---------------        (id, x <- Suma n1 n2)   
                Suma n1 n2
        
        Vale la propiedad                           

    CI2. n = Prod n1 n2)
            HI1) ¡ea2ExpA (expA2ea n1) = id n1!
            HI2) ¡ea2ExpA (expA2ea n2) = id n2!
            TI)  ¿ea2ExpA (expA2ea (Prod n1 n2)) = id (Prod n1 n2)?

        Dem.:
            Lado izq.
                 ea2ExpA (expA2ea (Prod n1 n2))
            =             --------------------  (expA2ea.3)
                 ea2ExpA (BOp Mul (expA2ea n1) (expA2ea n2)) 
                 -------------------------------------------
            =                                    (ea2ExpA.2)
                binOp2expAOp Mul (ea2ExpA (expA2ea n1)) (ea2ExpA (expA2ea n2))
            =   --------------------------------------------------------------     
                                                 (binOp2expAOp.2)
                Prod (ea2ExpA (expA2ea n1)) (ea2ExpA (expA2ea n2))
            =        ----------------------      (HI1)
                Prod (id n1) (ea2ExpA (expA2ea n2))
            =                ----------------------
                                                 (HI2)
                Prod (id n1) (id n2)
            =        ------  -------             (id)
                Prod n1 n2

            Lado der.
                 id (Prod n1 n2)  
            =    ---------------        (id, x <- Prod n1 n2)   
                Prod n1 n2

        Vale la propiedad para todos los casos
-}

{-
    ii. 
        Prop.: ¿expA2ea . ea2ExpA = id?
            Por ppio. de extensionalidad, es equivalente demostrar que:
                para todo e :: EA. ¿(expA2ea . ea2ExpA) e = id e?
            Por definición de compose, es equivalente demostrar que:
                para todo e :: EA. ¿expA2ea (ea2ExpA e) = id e?

        Sea n :: EA. Por ppio. de inducción sobre la estructura de n,
        es equivalente demostrar que
            ¿expA2ea (ea2ExpA n) = id n?
            
        CB. n = Const n) ¿expA2ea (ea2ExpA (Const n)) = id (Const n)?
        CI. n = BOp bop e1 e2)
                HI.1)   ¡expA2ea (ea2ExpA e1) = id e1!
                HI.2)   ¡expA2ea (ea2ExpA e2) = id e2!
                TI)     ¿expA2ea (ea2ExpA (BOp bop e1 e2)) = id (BOp bop e1 e2)?

        CB)
            Lado izq.
                expA2ea (ea2ExpA (Const n))
            =           ------------------          (ea2ExpA.1)
                expA2ea (Cte n)
            =   ---------------                     (expA2ea.1)
                Const n

            Lado der.
                id (Const n)
            =   -----------                         (id, x <- Const n)
                Const n
        Vale la propiedad

        CI)
            Lado izq.
                expA2ea (ea2ExpA (BOp bop e1 e2))
            =            -----------------------    (ea2ExpA.2)
                expA2ea (binOp2expAOp bop (ea2ExpA e1) (ea2ExpA e2))
            =   Lema distribucion expA2ea en BOp
                BOp bop (expA2ea (ea2ExpA e1)) (expA2ea (ea2ExpA e2))
            =           ---------------------------------------------
                                                    (HI.1, HI.2)
                BOp bop (id e1) (id e2)
            =           ------  ------- (id)
                BOp bop e1 e2

            Lema: Lema distribucion expA2ea en BOp
                ¿expA2ea (binOp2expAOp bop e1 e2) = BOp bop (expA2ea e1) (expA2ea e2)?



                                                    
-}
