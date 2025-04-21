-- EJERCICIO 4

data Medida = Mm Float   | Cm Float
            | Inch Float | Foot Float

asMM :: Medida -> Medida 
-- dada una medida cualquiera la
-- transforma en una medida en milímetros que aproxima la dada según la
-- conversión establecida
asMM (Mm n)     = Mm n
asMM (Cm n)     = Mm (n / 1000)
asMM (Inch n)   = Mm (n / 25.4)
asMM (Foot n)   = Mm (n / 30)