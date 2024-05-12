import Prelude hiding (maybe)

data Dungeon a =  Habitacion a
                  | Pasaje (Maybe a) (Dungeon a)
                  | Bifurcacion (Maybe a) 
                                 (Dungeon a)
                                 (Dungeon a)

-- Sea A un conjunto, el conjunto Dungeon A
-- está formado por las siguientes reglas

-- caso base)

-- Si x pertenece a A, entonces Habitacion x pertenece a Dungoen A

-- caso ind 1)

-- Si m pertenece al conjunto Maybe A,
--    y d pertenece al conjunto Dungeon A,
--    entonces Pasaje m d pertenece al conjunto Dungeon A

-- caso ind 2)

-- Si m pertenece al conjunto Maybe A,
--    y di pertenece al conjunto Dungeon A,
--    y dd pertenece al conjunto Dungeon A,
--    entonces Bifurcacion m di dd pertenece al conjunto Dungeon A

-- esquema de recursión estructural
-- sobre Dungeon
-- f (Habitacion x) = ...
-- f (Pasaje mx d) = ... f d
-- f (Bifurcacion mx di dd) =
-- 	... f di
-- 	... f dd

cantBif :: Dungeon a -> Int
cantBif (Habitacion x) = 0
cantBif (Pasaje mx d) = cantBif d
cantBif (Bifurcacion mx di dd) =
	1 + cantBif di + cantBif dd

cantPuntInter :: Dungeon a -> Int
cantPuntInter (Habitacion x) = 1
cantPuntInter (Pasaje mx d) =
   unoSiJust mx + cantPuntInter d
cantPuntInter (Bifurcacion mx di dd) =
   unoSiJust mx + cantPuntInter di + cantPuntInter dd

unoSiJust :: Maybe a -> Int
unoSiJust Nothing  = 0
unoSiJust (Just x) = 1

-- cantPuntInter :: Dungeon a -> Int
-- cantPuntInter (Habitacion x) = 1
-- cantPuntInter (Pasaje mx d) =
--    sumarUnoSiJust mx (cantPuntInter d)
-- cantPuntInter (Bifurcacion mx di dd) =
--    sumarUnoSiJust mx (cantPuntInter di + cantPuntInter dd)

-- sumarUnoSiJust Nothing  n = n
-- sumarUnoSiJust (Just x) n = 1 + n

-- sumarUnoSiJust Nothing  = id
-- sumarUnoSiJust (Just x) = (1+)

cantPuntosVacios :: Dungeon a -> Int
cantPuntosVacios (Habitacion x) = 0
cantPuntosVacios (Pasaje mx d) =
   unoSiNothing mx + cantPuntosVacios d
cantPuntosVacios (Bifurcacion mx di dd) =
   unoSiNothing mx + cantPuntosVacios di + cantPuntInter dd

-- version 1 y muy sensata y clara
-- unoSiNothing Nothing  = 1
-- unoSiNothing (Just x) = 0

unoSiNothing :: Maybe a -> Int
unoSiNothing = (1-) . unoSiJust

cantPuntosInterCon :: Eq a => a -> Dungeon a -> Int
cantPuntosInterCon e (Habitacion x) = 1
cantPuntosInterCon e (Pasaje mx d) =
   unoSiTiene e mx + cantPuntosInterCon e d
cantPuntosInterCon e (Bifurcacion mx di dd) =
   unoSiTiene e mx + cantPuntosInterCon e di + cantPuntosInterCon e dd


unoSiTiene e Nothing  = 0
unoSiTiene e (Just x) =
	if e == x then 1 else 0

----------------------------------------------
maybe :: b -> (a -> b) -> Maybe a -> b
maybe z f Nothing  = z
maybe z f (Just x) = f x

maybe_ :: b -> b -> Maybe a -> b
maybe_ z1 z2 = maybe z1 (const z2)

cumpleM :: (a -> Bool) -> Maybe a -> Bool
cumpleM p = maybe False p

boolToInt True  = 1
boolToInt False = 0

intToBool 1 = True
intToBool 0 = False

unoSiJust' = maybe_ 0 1
unoSiNothing' = maybe_ 1 0
unoSiTiene' e = boolToInt . cumpleM (e ==)
trueSiJust =  intToBool . unoSiJust'
--------------------------------------------

esLineal :: Dungeon a -> Bool
esLineal (Habitacion x) = True
esLineal (Pasaje mx d) = esLineal d
esLineal (Bifurcacion mx di dd) = False
	-- esto no hace falta
	-- && esLineal di
	-- && esLineal dd

tieneSoloBif :: Dungeon a -> Bool
tieneSoloBif (Habitacion x) = True
tieneSoloBif (Pasaje mx d) = False
tieneSoloBif (Bifurcacion mx di dd) = 
	esLineal di && esLineal dd

data Dir = L | S | R
data Path = End | D Dir Path

-- f End = ...
-- f (D d p) = ... f p

-- Tarea:
-- retorna true si hay algo al final del path
esInteresanteEn :: Path -> Dungeon a -> Bool
esInteresanteEn p        (Habitacion x)         = True
esInteresanteEn End      (Pasaje mx d)          = trueSiJust mx
esInteresanteEn End      (Bifurcacion mx di dd) = trueSiJust mx
esInteresanteEn (D di p) (Pasaje mx d)          = case di of
                                                   S -> esInteresanteEn p d
                                                   _ -> esInteresanteEn p (Pasaje mx d)
esInteresanteEn (D di p) (Bifurcacion mx dl dr) = case di of
                                                   L -> esInteresanteEn p dl
                                                   R -> esInteresanteEn p dr
                                                   _ -> esInteresanteEn p (Bifurcacion mx dl dr)

llenoDe :: Eq a => a -> Dungeon a -> Bool
llenoDe e (Habitacion x) = e == x
llenoDe e (Pasaje mx d)  = 
   maybe False (e ==) mx && llenoDe e d
llenoDe e (Bifurcacion mx di dd) =
   maybe False (e ==) mx
     && llenoDe e di
     && llenoDe e dd

-- Desafío/Tarea:
-- replaces the elements of the dungeon
-- with the path to each element
--replaceWithPaths :: Dungeon a -> Dungeon Path
--replaceWithPaths (Habitacion x) = ...
--replaceWithPaths (Pasaje mx d) = ... replaceWithPaths d
--replaceWithPaths (Bifurcacion mx di dd) = ... replaceWithPaths di
--                                        	... replaceWithPaths dd
