type Nombre = String 
data Planilla = Fin | Registro Nombre Planilla 
data Equipo = Becario Nombre | Investigador Nombre Equipo Equipo Equipo

{-
    Reglas:
        Reglas base: Fin está en Planilla
                     Becario n está en Equipo
        Reglas inductivas:
                    Si p está en Planilla
                        entonces Registro Nombre p está en Planilla
                    Si e1, e2 y e3 están en Equipo
                        entonces Investigador Nombre e1 e2 e3 está en Equipo

    Forma esquemática

        f :: Planilla -> a
        f Fin = ...
        f (Registro n p) = ... f p ...

        f' :: Equipo -> a
        f' (Becario n) = ...
        f' (Investigador n e1 e2 e3) = ... f' e1 ... f' e2 ... f' e3 ...
-}

largoDePlanilla :: Planilla -> Int 
largoDePlanilla Fin     	   = 0
largoDePlanilla (Registro _ p) = 1 + largoDePlanilla 

esta :: Nombre -> Planilla -> Bool 
esta _ Fin             = False 
esta n (Registro n' p) = n == n' || esta n p 

juntarPlanillas :: Planilla -> Planilla -> Planilla 
juntarPlanillas Fin p2            = p2 
juntarPlanillas (Registro n p) p2 = Registro n (juntarPlanillas p p2)

nivelesJerarquicos :: Equipo -> Int 
nivelesJerarquicos (Becario _) = 1
nivelesJerarquicos (Investigador _ e1 e2 e3) = 1 +
        max (nivelesJerarquicos e1) 
            (max (nivelesJerarquicos e2) (nivelesJerarquicos e3))

cantidadDeIntegrantes :: Equipo -> Int 
cantidadDeIntegrantes (Becario _) = 1
cantidadDeIntegrantes (Investigador _ e1 e2 e3) = 
        1 + (cantidadDeIntegrantes e1) + (cantidadDeIntegrantes e2) + (cantidadDeIntegrantes e3)

planillaDeIntegrantes :: Equipo -> Planilla 
planillaDeIntegrantes (Becario n) = Registro n Fin 
planillaDeIntegrantes (Investigador n e1 e2 e3) =
        Registro n (juntarPlanillas (planillaDeIntegrantes e1)
            (juntarPlanillas (planillaDeIntegrantes e2) (planillaDeIntegrantes e3)))


