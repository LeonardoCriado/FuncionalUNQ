-- Hasta ahora:
-- Funciones (alto orden)
-- Sistema de Tipos
-- Reducciones
-- Demostraciones 
-- (principio de extensionalidad)

-- Nuevo tema
-- Recursión estructural (explícita)

-- tipo definido recursivamente
data Pizza = Prepizza
           | Capa Ingrediente Pizza

-- Definición de Pizza como conjunto inductivo

-- Vamos a definir el conjunto Pizza
-- formado por las siguientes reglas:

-- regla/caso base)

-- * Prepizza está en conjunto Pizza

-- regla/caso inductivo)

-- * Sea i un elemento del conjunto Ingrediente
--   y p un elemento del conjunto Pizza,
--   entonces Capa i p está en el conjunto Pizza

data Ingrediente = Aceitunas Int 
                 | Cebolla
                 | Jamon
                 | Queso
                 | Salsa
                 | Ricota

-- esquema de recursión estructura
-- sobre... Pizza
-- f Prepizza = ...
-- f (Capa i p) = ... f p


cantCapas :: Pizza -> Int
cantCapas Prepizza   = 0
cantCapas (Capa i p) = 1 + cantCapas p

-- cantAceitunas :: Pizza -> Int
-- cantAceitunas Prepizza   = ...
-- cantAceitunas (Capa i p) = 
-- 	if esAceitunas i
-- 	   then aceitunas i + cantAceitunas p
-- 	   else cantAceitunas p

-- esAceitunas (Aceitunas n) = True
-- esAceitunas _ = False

-- aceitunas (Aceitunas n) = n
-- aceitunas _ = error "no tiene aceitunas"

cantAceitunas :: Pizza -> Int
cantAceitunas Prepizza   = 0
cantAceitunas (Capa i p) = 
	aceitunas i + cantAceitunas p

aceitunas (Aceitunas n) = n
aceitunas _             = 0


dupAceitunas :: Pizza -> Pizza
dupAceitunas Prepizza = Prepizza
dupAceitunas (Capa i p) = 
	Capa (dupAc i) (dupAceitunas p)

dupAc (Aceitunas n) = Aceitunas (n*2)
dupAc i             = i

sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza = Prepizza
sinLactosa (Capa i p) = 
	agregarSinLactosa i (sinLactosa p)

-- agregarSinLactosa :: Ingrediente -> Pizza -> Pizza
-- agregarSinLactosa Queso p = p
-- agregarSinLactosa i     p = Capa i p

agregarSinLactosa :: Ingrediente -> Pizza -> Pizza
agregarSinLactosa i p =
	if contieneLactosa i
	   then p
	   else Capa i p

contieneLactosa :: Ingrediente -> Bool
contieneLactosa Queso  = True
contieneLactosa Ricota = True
contieneLactosa _      = False

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa Prepizza = True
aptaIntolerantesLactosa (Capa i p) = 
	not (contieneLactosa i) && aptaIntolerantesLactosa p

conAceitunasJuntas :: Pizza -> Pizza
conAceitunasJuntas Prepizza = Prepizza
conAceitunasJuntas (Capa i p) = 
	juntar i (conAceitunasJuntas p)

juntar (Aceitunas n) (Capa (Aceitunas m) p) =
	Capa (Aceitunas (n + m)) p
juntar i p = Capa i p

-- conAceitunasJuntas (
-- 	Capa Jamon 
-- 	 (Capa (Aceitunas 10) 
-- 	   (Capa (Aceitunas 20) 
-- 	  	(Capa Queso Prepizza)))
-- )
-- ->
-- 	Capa Jamon 
-- 	 (Capa (Aceitunas 30) 
-- 	  	(Capa Queso Prepizza))

-- conAceitunasJuntas (
-- 	Capa Jamon 
-- 	 (Capa (Aceitunas 10) 
-- 	   (Capa (Aceitunas 20) 
-- 	  	(Capa Queso 
-- 	  		(Capa (Aceitunas 5) Prepizza))))
-- )
-- ->
-- 	Capa Jamon 
-- 	 (Capa (Aceitunas 30) 
-- 	  	(Capa Queso 
-- 	  		(Capa (Aceitunas 5) Prepizza)))


CI) cantDeAcs( dupAscs (Capa i q'))

EJ 1)

a)

length :: [a] -> Int
length  []     = 0
length  (x:xs) = 1 + length xs

sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs


-- binario 01 = 2

type NBin = [DigBin]
data DigBin = I | O

--Ej 3 
-- iii

succNB:: NBin -> NBin
succNB []     = [I]
succNB (x:xs) = case x of
                I -> O : succNB xs
                O -> I : xs

addNB :: NBin -> NBin -> NBin
addNB []     []     = ...
addNB (x:xs) (y:ys) = 