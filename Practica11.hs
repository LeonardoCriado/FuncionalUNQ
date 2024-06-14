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

cantidadCapasQueCumplen:: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen = flip cantidadCapasQueCumplen2

cantidadCapasQueCumplen2:: Pizza -> (Ingrediente -> Bool) -> Int
cantidadCapasQueCumplen2 Prepizza   f = 0
cantidadCapasQueCumplen2 (Capa i p) f = boolToint (f i) + cantidadCapasQueCumplen2 p f

boolToint True  = 1
boolToint False = 0


conCapasTransformadas:: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas f Prepizza   = Prepizza
conCapasTransformadas f (Capa i p) = Capa (f i) (conCapasTransformadas f p)


applyIf :: (a -> b -> b) -> (a -> Bool) -> a -> b -> b
applyIf f g x y = if g x then f x y else y

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
cantidadCapasQueCumplen' f = pizzaProcesada (\i -> \n -> boolToint (f i) + n)  0
--b = (Ingrediente -> Bool) -> Int
--f :: (Ingrediente -> ((Ingrediente -> Bool) -> Int) -> ((Ingrediente -> Bool) -> Int))


conCapasTransformadas':: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas' = flip (pizzaProcesada (\i -> \h -> \f -> Capa (f i) (h f) ) (const Prepizza))
--b = (Ingrediente -> Ingrediente) -> Pizza


soloLasCapasQue:: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue = flip (pizzaProcesada (\i -> \h -> \f -> (applyIf (Capa)) f i (h f)) (const Prepizza))

-- b = (Ingrediente -> Bool) -> Pizza
-- pizzaProcesada::(Ingrediente -> ((Ingrediente -> Bool) -> Pizza) -> ((Ingrediente -> Bool) -> Pizza)) -> ((Ingrediente -> Bool) -> Pizza) -> Pizza -> ((Ingrediente -> Bool) -> Pizza)

sinLactosa' :: Pizza -> Pizza
sinLactosa' = pizzaProcesada ((applyIf (Capa)) (not . tieneLactosa)) Prepizza

-- b = Pizza
-- pizzaProcesada::(Ingrediente -> Pizza -> Pizza) -> Pizza -> Pizza -> Pizza

aptaIntolerantesLactosa' :: Pizza -> Bool
aptaIntolerantesLactosa' = pizzaProcesada ((&&) . (not . tieneLactosa)) True

-- b = Bool
--pizzaProcesada:: (Ingrediente -> Bool -> Bool) -> Bool -> Pizza -> Bool

cantidadDeQueso' :: Pizza -> Int
cantidadDeQueso' = pizzaProcesada ((+) . (boolToint . esQueso)) 0
-- b = Int
-- pizzaProcesada:: (Ingrediente -> Int -> Int) -> Int -> Pizza -> Int

conElDobleDeAceitunas' :: Pizza -> Pizza
conElDobleDeAceitunas' = pizzaProcesada (Capa . duplicarAceitunas) Prepizza

-- b = Pizza
--pizzaProcesada:: (Ingrediente -> Pizza -> Pizza) -> Pizza -> Pizza -> Pizza

-- Ejercicio 5)

cantidadAceitunas :: Pizza -> Int
cantidadAceitunas = pizzaProcesada ((+) . numeroAceitunas) 0

numeroAceitunas :: Ingrediente -> Int
numeroAceitunas (Aceitunas i) = i
numeroAceitunas _             = 0

-- b = Pizza
-- pizzaProcesada:: (Ingrediente -> Pizza -> Pizza) -> Pizza -> Pizza -> Pizza

capasQueCumplen:: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen = flip (pizzaProcesada (\i -> \r -> \f -> (applyIf (:)) f i (r f)) (const []))

-- b = (Ingrediente -> Bool) -> [Ingrediente]
-- pizzaProcesada:: (Ingrediente -> ((Ingrediente -> Bool) -> [Ingrediente]) -> ((Ingrediente -> Bool) -> [Ingrediente])) 
--                  -> ((Ingrediente -> Bool) -> [Ingrediente]) -> Pizza 
--                  -> ((Ingrediente -> Bool) -> [Ingrediente]) 


conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada = pizzaProcesada sumarAceitunas Prepizza

sumarAceitunas :: Ingrediente -> Pizza -> Pizza
sumarAceitunas (Aceitunas x) (Capa (Aceitunas i) p) = Capa (Aceitunas (i+x)) p
sumarAceitunas i             p                      = Capa i                 p


conCapasDe :: Pizza -> Pizza -> Pizza -- que agrega las capas de la primera pizza sobre la segunda
conCapasDe = pizzaProcesada ((.) (.) Capa) id

-- b = Pizza -> Pizza
-- pizzaProcesada:: (Ingrediente -> (Pizza -> Pizza) -> (Pizza -> Pizza)) 
--                  -> (Pizza -> Pizza) -> Pizza -> (Pizza -> Pizza)


---primerasNCapas :: Int -> Pizza -> Pizza





e1 = Capa Jamon
 	 (Capa (Aceitunas 10) 
 	   (Capa Ricota 
 	  	(Capa Queso 
 	  		(Capa (Aceitunas 5) 
          (Capa (Aceitunas 5) 
               Prepizza)))))