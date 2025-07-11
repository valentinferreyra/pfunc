data AppList a = Single a 
            |    Append (AppList a) (AppList a)

-- describe la cantidad de elementos de la lista
lenAL :: AppList a -> Int 
lenAL (Single l)      = 1 
lenAL (Append al1 al2) = lenAL al1 + lenAL al2 

-- describe la lista resultante de agregar el elemento dado
-- al principio de la lista
consAL :: a -> AppList a -> AppList a 
lenAL x (Single l)       = Append (Single x) (Single l)
lenAL x (Append al1 al2) = Append (consAL x al1) al2

-- describe el primer elemento de la lista dada
headAL :: AppList a -> a 
headAL (Single l)       = l
headAL (Append al1 al2) = headAL al1 

-- describe el resultante de quitar el primer elemento de la lista dada
tailAL :: AppList a -> AppList a
tailAL (Single l)       = error ""
tailAL (Append al1 al2) = tailAL' (tailAL al1) (tailAL al2)

tailAL' :: AppList a -> AppList a -> AppList a
tailAL' (Single l) appl       = appl  
tail    (Append al1 al2) app1 = Append al1 al2

-- describe la lista resultante de agregar el elemento dado al final
-- de la lista dada
snocAL :: AppList a -> a -> AppList a
snocAL (Single l) x       = Append (Single l) (Single x)
snocAL (Append al1 al2) x = Append al1 (snocAL al2 x)

-- describe el ultimo elemento de la lista dada
lastAL :: AppList a -> a 
lastAL (Single l)       = l  
lastAL (Append al1 al2) = lastAL al2

-- describe la lista dada sin su ultimo elemento
initAL :: AppList a -> AppList a 
initAL (Single l)       = error ""
initAL (Append al1 al2) = initAL' al1 al2 

initAL' :: AppList a -> AppList a -> AppList a 
initAL' apl (Single _) = apl 
initAL' apl apl'       = Append apl apl'

-- describe la lista dada con sus elementos en orden inverso
reverseAL :: AppList a -> AppList a 
reverseAL (Single l)       = Single l 
reverseAL (Append al1 al2) = Append (reverseAL al2) (reverseAL al1)

-- indica si el elemento dado se encuentra en la lista dada
elemAL :: Eq a => a -> AppList a -> Bool 
elemAL x (Single l)         = x == l
elemAL x (Append al1 al2)   = elemAL x al1 || elemAL x al2

-- describe el resultado de agregar los elementos de la primera lista
-- adelanet de los elementos de la segunda
appendAL :: AppList a -> AppList a -> AppList a 
appendAL al al'        = Append al al'

-- describe la representacion lineal de la lista dada
appListToList :: AppList a -> [a]
appListToList (Single l)       = [l]
appListToList (Append al1 al2) =
        appListToList al1 ++ appListToList al2 


-- -----------------------------------------------------------
{-
    Prop.: para todo xs :: AppList a. para todo ys :: AppList a.
        ¿lenAL (appendAL xs ys) = lenAL xs + lenAL ys?
    Dem. : 
        Sea x :: AppList a. y :: AppList a.
        ¿lenAL (appendAL x y) = lenAL x + lenAL y?
    Por ppio de induccion sobre la estructura de x, es equivalente demostrar que:

    Caso base. x = Single l)
                ¿lenAL (appendAL (Single l) y) = lenAL (Single l) + lenAL y?
    Caso inductivo. x = Append l1 l2)
                HI.1) ¡lenAL (appendAL l1 y) = lenAL l1 + lenAL y!
                HI.2) ¡lenAL (appendAL l2 y) = lenAL l2 + lenAL y!
                TI.) ¿lenAL (appendAL (Append l1 l2) y) = lenAL (Append l1 l2) + lenAL y?

    Dem. caso base)
        Lado izq.:
                lenAL (appendAL (Single l) y)
            =          --------------------- (appendAL)
                lenAL (Append (Single l) y)
            =  ----------------------------- (lenAL.2)
                lenAL (Single l) + lenAL y
            =   ---------------              (lenAL.1)
                1 + lenAL y
        
        Lado der.:
                lenAL (Single l) + lenAL y
            =   ---------------             (lenAL.1)
            |   1 + lenAL y
        
        Vale la propiedad.


    Dem. caso inductivo)
        Lado izq.:
                lenAL (appendAL (Append l1 l2) y)
            =          ------------------------- (appendAL)
                lenAL (Append (Append l1 l2) y)
            =   -------------------------------  (lenAL.2)
                lenAL (Append l1 l2) + lenAL y
        
        Lado der.:
            lenAL (Append l1 l2) + lenAL y

    Vale la propiedad
-}

-- -----------------------------------------------------
{-
    Prop.: ¿reverseAL . reverseAL = id?
    Dem. : Por ppio. de extensionalidad, es equivalente demostrar que:
        Para todo x :: AppList a,
            ¿(reverseAL . reverseAL) x = id x?
        por definicion de compose, es equivalente demostrar que:
            ¿reverseAL (reverseAL x) = id x?
        
        Sea xs un AppList a cualquiera, 
        Por ppio de inducción sobre la estrcutura de xs, es equivalente demostrar:
            ¿reverseAL (reverseAL xs) = id xs?
    
        Caso base. xs = Single l)
            ¿reverseAL (reverseAL (Single l)) = id (Single l)?
        Caso inductivo. xs = Append l1 l2)  
            HI.1) ¡reverseAL (reverseAL l1) = id l1!
            HI.2) ¡reverseAL (reverseAL l2) = id l2!
            TI) ¿reverseAL (reverseAL (Append l1 l2)) = id (Append l1 l2)?

    Dem. caso base)
        Lado izq.:
                reverseAL (reverseAL (Single l))
            =              -------------------- (reverseAL.1)
                reverseAL (Single l)
            =   -------------------             (reverseAL.1)
                Single l
        
        Lado der.:
                id (Single l)
            =   ------------- (id)
                Single l

        Vale la propiedad.

    Dem. caso inductivo)
        Lado izq.:
                reverseAL (reverseAL (Append l1 l2))
            =             -------------------------- (reverseAL.2)
                reverseAL (Append (reverseAL l2) (reverseAL l1))
            =   ------------------------------------------------(reverseAL.2)
                Append (reverse (reverseAL l1)) (reverse (reverseAL l2))
            = HI, HI2
                Append (id l1) (id l2)
            = id
                Append l1 l2
        
        Lado der.:
                id (Append l1 l2)
            = id
                Append l1 l2
    Vale la propiedad

-}

-- --------------------------------------------------------
{-
    Prop.: ¿(reverseAL .) . flip consAL . reverseAL = snocAL?

-}