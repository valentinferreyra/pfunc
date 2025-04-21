-- EJERCICIO 3

data DigDec = D0 | D1 | D2 | D3 | D4
            | D5 | D6 | D7 | D8 | D9
        deriving Show


ddAsInt :: DigDec -> Int
-- dado un símbolo que representa un dígito decimal lo transforma en su significado como número
ddAsInt D0 = 0
ddAsInt D1 = 1
ddAsInt D2 = 2
ddAsInt D3 = 3
ddAsInt D4 = 4
ddAsInt D5 = 5
ddAsInt D6 = 6
ddAsInt D7 = 7
ddAsInt D8 = 8 
ddAsInt D9 = 9

ddOfInt :: Int -> DigDec 
-- dado un número entre 0 y 9 lo  transforma en el símbolo que representa a ese dígito
ddOfInt 1 = D1
ddOfInt 2 = D2
ddOfInt 3 = D3
ddOfInt 4 = D4
ddOfInt 5 = D5
ddOfInt 6 = D6
ddOfInt 7 = D7
ddOfInt 8 = D8
ddOfInt 9 = D9
ddOfInt 0 = D0
ddOfInt _ = error "no es un dd"

nextDD :: DigDec -> DigDec 
-- dado un dígito decimal lo transforma en el siguiente según el orden circular dado en la definición
nextDD D9 = D0
nextDD dd = ddOfInt (ddAsInt dd + 1)

prevDD :: DigDec -> DigDec
-- dado un dígito decimal lo transforma en el anterior según el orden circular dado en la definición
prevDD D0 = D9 
prevDD dd = ddOfInt (ddAsInt dd - 1) 