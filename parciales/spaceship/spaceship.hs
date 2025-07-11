data Component = Cargo | Engine | Shield | Cannon 
data Spaceship = Module Component Spaceship Spaceship | Plug 

data Direction = Larboard | Starboard 
data Size = Small | Big | Torpedo 
type Hazard = (Direction, Int, Size)

-- EJERCICIO 1
shielded :: Spaceship -> Bool 
shielded Plug = False 
shielded (Module c s1 s2) = isShield c || shielded s1 || shielded s2  

isShield :: Component -> Bool 
isShield Shield = True 
isShield _      = False  

armed :: Spaceship -> Bool 
armed Plug = False 
armed (Module c s1 s2) = isCannon c || shielded s1 || shielded s2 

isCannon :: Component -> Bool 
isCannon Cannon = True 
isCannon _      = False  

thrust :: Spaceship -> Int 
thrust Plug = 0 
thrust (Module c s1 s2) = unoSiPropulsa c  + thrust s1 + thrust s2

unoSiPropulsa :: Component -> Int 
unoSiPropulsa Engine = 1 
unoSiPropulsa _      = 0

-- devuelve la nave resultante de desprender los módulos dependientes
-- del módulo donde se recibe el impacto
wreck :: Hazard -> Spaceship -> Spaceship 
wreck h ss = let h' = shootHazardIf h (armed ss) in 
    if isSmallH h' && armed ss 
        then ss 
        else wreck' h' ss 

wreck' :: Hazard -> Spaceship -> Spaceship 
wreck' _ Plug = Plug 
wreck' (d, 1, n) _ = Plug -- si es 1, le pega en la cabina
wreck' (d, n, s) (Module c ss1 ss2) = case d of 
    Larboard -> Module c (wreck' (d, n-1, s) ss1) ss2 
    Starboard ->  Module c ss1 (wreck' (d, n-1, s) ss2)

isSmallH :: Hazard -> Bool 
isSmallH (d, n, Small) = True 
isSmallH _ = False 

shootHazardIf :: Hazard -> Bool -> Hazard
shootHazardIf (d, n, Big) True = (d, n, Small)
shootHazardIf h _              =  h

-- De el tipo y una implementación para una función que generalice
-- la recursión sobre las naves espaciales (foldSS)
foldSS :: b -> (Component -> b -> b -> b) -> Spaceship -> b 
foldSS cb cr Plug = cb 
foldSS cb cr (Module c s1 s2) = cr c (foldSS cb cr s1) (foldSS cb cr s2)

-- retorna la capacidad de la nave, donde cada modulo de carga
-- aporta una unidad de capacidad
capacity :: Spaceship -> Int 
capacity = foldSS 0 (\x n1 n2 -> unoSi (esCargo x) + n1 + n2)

esCargo :: Component -> Bool 
esCargo Cargo = True 
esCargo _ = False 

unoSi :: Bool -> Int 
unoSi True = 1
unoSi False = 0

-- dada una lista de naves retorna una de capacidad maxima
largest :: [Spaceship] -> Spaceship
largest = recr (error "No hay naves en la lista")
               (\s ss r -> if null ss 
                            then s
                            else if capacity s > capacity r 
                                then s else r)

-- dada una nave retorna su alto y ancho (pensando el alto como la cantidad
-- de componentes de la rama mas larga y el ancho como la cantidad de componentes
-- del nivel mas ancho)
dimensions :: Spaceship -> (Int, Int) 
dimensions = appFork (heightSS, maxSize . map length . levelsSS)

heightSS :: Spaceship -> Int 
heightSS = foldSS 0 (\x n1 n2 -> 1 + max n1 n2)

levelsSS :: Spaceship -> [[Component]]
levelsSS = foldSS [] (\x cc1 cc2 -> [x] : mergeLevelsSS cc1 cc2)

mergeLevelsSS :: [[Component]] -> [[Component]] -> [[Component]]
mergeLevelsSS = recr id (\xs xss rss -> yss -> 
    case yss of 
        [] -> xs : xss 
        (ys:yss') -> (xs ++ ys) : (rss yss'))

-- simula el resultado de maniobrar una nave a través de una serie de peligros,
-- si se encuentra un objeto pequeño y la nave está escudada no produce impacto, 
-- si el objeto es grande y la nave está armada entonces se transforma en un objeto pequeño.
-- si es un torpedo no se puede evitar el impacto
manoeuvre :: Spaceship -> [Hazard] -> Spaceship
manoeuvre = flip (foldr id
                        (\h hs -> s -> hs (wreck h s))) -- hs :: Spaceship -> Spaceship por el flip

-- dada una lista de naves y una lista de peligros retorna la lista de naves
-- que sobreviven los peligros (es decir las naves con motores funcionales luego de navegar 
-- a traves de los meteoros)
test :: [Spaceship] -> [Hazard] -> [Spaceship]
test = flip (filter . survives)

survives :: [Hazard] -> Spaceship -> Bool 
survives hs s = let s' = manoeuvre s hs 
    in foldSS False (\c b b' -> isEngine c || b || b') s'

-- Demostrar
components :: Spaceship -> [Component]
components Plug = []
components (Module c s1 s2) = components s1 ++ [c] ++ components s2 

replace :: (Component -> Component) -> Spaceship -> Spaceship
replace f Plug = Plug
replace f (Module c s1 s2) = Module (f c) (replace f s1) (replace f s2)

-- Demostrar
{-
    para todo sp: Spaceship. components (replace f sp) = map f (components sp)

    sea s :: Spaceship, ¿components (replace f s) = map f (components s)?
    Por ppio. de induccion sobre la estructura de Spaceship. es equivalente demostrar:

    CB. s = Plug)
            ¿ components (replace f Plug) = map f (components Plug) ?

    CI. s = Module c s1 s2)
        TI.1) ¡components (replace f s1) = map f (components s1)!
        TI.2) ¡components (replace f s2) = map f (components s2)!
        HI) 
            ¿ components (replace f (Module c s1 s2)) = map f (components (Module c s1 s2)) ?

    Dem. CB)
        Lado izq.
            components (replace f Plug)
        ->                              (replace.1)
            components Plug
        ->                              (components.1)
            []
        
        Lado der.
            map f (components Plug)
        ->                              (components.1)
            map f []
        ->                              (map.1)
            []
        
    Vale la propiedad.

    Dem. CI)
        Lado izq.
            components (replace f (Module c s1 s2))
        ->                              (replace.2)
            components (Module (f c) (replace f s1) (replace f s2))
        -> Lema.1
            components (replace f s1) ++ map f [c] ++ components (replace f s2)

        
        Lado der.
            map f (components (Module c s1 s2))
        -> Lema distribuidad del map sobre ++
            map f (components s1 ++ [c] ++ components s2)
        ->
            map f components s1 ++ map f [c] ++ map f components s2
        -> TI.1 , TI.2
            components (replace f s1) ++ map f [c] ++ components (replace f s2)
-}