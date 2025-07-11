pizzaProcesada :: a -> (Ingrediente -> a -> a) -> Pizza -> a 
pizzaProcesada bp fi Prepizza = bp 
pizzaProcesada bp fi (Capa i p) = fi i (pizzaProcesada bp fi p)

cantCapas :: Pizza -> Int 
cantCapas = pizzaProcesada 0 (\i r -> 1 + r)

cantAceitunas :: Pizza -> Int 
cantAceitunas = pizzaProcesada 0 (\i r -> aceitunas i + r)

cambiarTodoPorQueso :: Pizza -> Pizza
cambiarTodoPorQueso = pizzaProcesada Prepizza (\i r -> Capa Queso r)

