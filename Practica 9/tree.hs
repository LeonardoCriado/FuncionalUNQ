data Tree a = EmptyT 
            | NodeT a (Tree a) (Tree a)

--f EmptyT = ...
--f (NodeT x ti td) =
--	... f ti
--	... f td

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x ti td) = 1 + (heightT ti `maxi` heightT td)

maxi:: Int -> Int -> Int
maxi x y = if x > y then x else y

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x ti td) = [x] : (juntarPorNivel (listPerLevel ti)  (listPerLevel td))

juntarPorNivel :: [[a]] -> [[a]] -> [[a]]
juntarPorNivel []     []       = []
juntarPorNivel []     r2       = r2
juntarPorNivel r1     []       = r1
juntarPorNivel (n:ns) (n2:ns2) = (n ++ n2) : juntarPorNivel ns ns2

levelN :: Int -> Tree a -> [a]
levelN 0 EmptyT = []
levelN 0 (NodeT x ti td) = [x]
levelN n EmptyT = []	
levelN n (NodeT x ti td) = levelN (n-1) ti ++ levelN (n-1) td

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x ti td) = x : listaMasLarga (ramaMasLarga ti)  (ramaMasLarga td)

listaMasLarga :: [a] -> [a] -> [a]
listaMasLarga l1 l2 = if length l1 > length l2
							then l1
							else l2	

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x ti td) = consATodas x (todosLosCaminos ti ++ todosLosCaminos td)

consATodas :: a -> [[a]] -> [[a]]	
consATodas x [] 	  = [[x]]
consATodas x (xs:xss) = (x : xs) : consATodas x xss

-- ¿ heightT = length . ramaMasLarga ?

Por ppio de ext:
Para todo x. ¿ heightT x = (length . ramaMasLarga) x ?

por def de (.)
Para todo x. ¿ heightT x = length (ramaMasLarga x) ?

Sea t un "Tree a", siendo a un elemento cualquiera. Por induccion en la estructura de t:

Caso Base t = EmptyT)
¿ heightT EmptyT = length (ramaMasLarga EmptyT) ?

IZQ)
heightT EmptyT
=   			
0

DER)
length (ramaMasLarga EmptyT) 
= 						(ramaMasLarga)
length []
=
0



Caso Inductivo t = NodeT a t1 t2) 
HI1) ¡ heightT t1 = length (ramaMasLarga t1) !
HI2) ¡ heightT t2 = length (ramaMasLarga t2) !
TI) ¿ heightT (NodeT a t1 t2) = length (ramaMasLarga (NodeT a t1 t2)) ?


IZQ)
heightT (NodeT a t1 t2)
=                                                                  (heightT)
1 + (heightT t1 `maxi` heightT t2)
=					                                               (HI 1 y 2)
1 + (length (ramaMasLarga t1) `maxi` length (ramaMasLarga t2))


DER)
length (ramaMasLarga (NodeT a t1 t2))
=                                                                   (ramaMasLarga)	
length(a : listaMasLarga (ramaMasLarga t1) (ramaMasLarga t2))
=                                                                   (length)
1 + ( length (listaMasLarga (ramaMasLarga t1) (ramaMasLarga t2)))
=                                                                   LEMA: length (listaMasLarga x y) = length x `maxi` length y
1 + (length (ramaMasLarga t1) `maxi` length (ramaMasLarga t2))


Para todo x, para todo y.
¿length (listaMasLarga x y) = length x `maxi` length y?

CASO 1) length x > length y

IZQ)
length (listaMasLarga x y)
= 								(listaMasLarga) 
length x

DER)
length x `maxi` length y
= 								(maxi)
length x

CASO 2) length x <= length y

IZQ)
length (listaMasLarga x y)
= 								(listaMasLarga) 
length y

DER)
length x `maxi` length y
= 								(maxi)
length y



-- Sugerencia:

-- lema
-- se demuestra por caso


-- tarea
-- Para todo f, x, y. 
--   f (if b then x else y) = if b then f x else f y


-- y no dividir por casos