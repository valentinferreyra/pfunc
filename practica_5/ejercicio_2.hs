-- EJERCICIO 2

data DigBin = O | I 
    deriving Show

dbAsInt :: DigBin -> Int
-- dado un símbolo que representa un dígito binario lo transforma en su significado como número
dbAsInt O = 0
dbAsInt I = 1

dbAsBool :: DigBin -> Bool 
-- que dado un símbolo que representa un dígito binario lo transforma en su significado como booleano
dbAsBool O = False 
dbAsBool I = True

dbOfBool :: Bool -> DigBin 
-- dado un booleano lo transforma en el símbolo que representa a ese booleano
dbOfBool b = if b then I else O 

negDB :: DigBin -> DigBin 
-- que dado un dígito binario lo transforma en el otro.
negDB O = I
negDB I = O