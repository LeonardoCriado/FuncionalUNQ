data Pizza = Prepizza
           | Capa Ingrediente Pizza
           deriving Show

data Ingrediente = Aceitunas Int 
                 | Cebolla
                 | Jamon
                 | Queso
                 | Salsa
                 | Ricota
                deriving Show
-- f Prepizza   = ...
-- f (Capa i p) = ... f p

-- Ej 1)



boolToint True  = 1
boolToint False = 0


conCapasTransformadas:: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas f Prepizza   = Prepizza
conCapasTransformadas f (Capa i p) = Capa (f i) (conCapasTransformadas f p)

soloLasCapasQue:: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue f Prepizza   = Prepizza
soloLasCapasQue f (Capa i p) = capaSi f i (soloLasCapasQue f p)

capaSi:: (Ingrediente -> Bool) -> Ingrediente -> Pizza -> Pizza
capaSi f i p = if f i then Capa i p else p


-- Ej 2)

-- Ej 2) Definir las siguientes funciones utilizando alguna de las definiciones
-- anteriores:
sinLactosa :: Pizza -> Pizza
sinLactosa = soloLasCapasQue (not . tieneLactosa)

tieneLactosa :: Ingrediente -> Bool
tieneLactosa Queso  = True
tieneLactosa Ricota = True
tieneLactosa _      = False

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa = (<1) . (cantidadCapasQueCumplen tieneLactosa)

cantidadDeQueso :: Pizza -> Int
cantidadDeQueso = cantidadCapasQueCumplen esQueso

esQueso Queso = True
esQueso _     = False

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas = conCapasTransformadas duplicarAceitunas

duplicarAceitunas:: Ingrediente -> Ingrediente
duplicarAceitunas (Aceitunas i) = Aceitunas (i*2)
duplicarAceitunas i             = i

-- Ej 3
pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada f b Prepizza   = b
pizzaProcesada f b (Capa i p) = f i (pizzaProcesada f b p)

-- Ej 4
cantidadCapasQueCumplen':: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen' = pizzaProcesada (\i -> \h -> \f -> boolToint (f i) + (flip h) f)  (flip (\f -> \g -> 0))

cantidadCapasQueCumplen:: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen f Prepizza   = 0
cantidadCapasQueCumplen f (Capa i p) = boolToint (f i) + flip cantidadCapasQueCumplen p f

--(flip (\f -> 0))
--(\f -> \i -> \h-> boolToint (f i) + flip h f)  | f i (cantidadCapasQueCumplen p) 

--conCapasTransformadas':: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
--conCapasTransformadas' = pizzaProcesada ((.) Capa ) (flip (\f -> \g -> Prepizza))

--soloLasCapasQue':: (Ingrediente -> Bool) -> Pizza -> Pizza
















e1 = Capa Jamon
 	 (Capa (Aceitunas 10) 
 	   (Capa Ricota 
 	  	(Capa Queso 
 	  		(Capa (Aceitunas 5) 
               Prepizza))))