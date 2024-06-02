data Tree a = EmptyT 
            | NodeT a (Tree a) (Tree a)
            deriving Show

-- f EmptyT = ...
-- f (NodeT x ti td) =
-- 	... f ti
-- 	... f td

ejemplo :: Tree Int
ejemplo =
	NodeT 1
		(NodeT 2
			(NodeT 4 EmptyT EmptyT)
			(NodeT 5 EmptyT EmptyT))
		(NodeT 3
			(NodeT 6 EmptyT EmptyT)
			(NodeT 7 EmptyT 
				     (NodeT 8 EmptyT EmptyT)))

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x ti td) =
	1 + max (heightT ti) (heightT td)

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x ti td) =
	[x] : concatPerPos (listPerLevel ti) (listPerLevel td)

concatPerPos :: [[a]] -> [[a]] -> [[a]]
concatPerPos [] yss = yss
concatPerPos xss [] = xss
concatPerPos (xs:xss) (ys:yss) =
	(xs ++ ys) : concatPerPos xss yss

levelN :: Int -> Tree a -> [a]
levelN n EmptyT = []
levelN 0 (NodeT x ti td) = [x]
levelN n (NodeT x ti td) =
	levelN (n-1) ti ++ levelN (n-1) td

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x ti td) =
	if length (ramaMasLarga ti) > length (ramaMasLarga td)
       then x : ramaMasLarga ti
       else x : ramaMasLarga td

	-- if heightT ti > heightT td 
    --    then x : ramaMasLarga ti
    --    else x : ramaMasLarga td

-- todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
-- todosLosCaminos (NodeT x EmptyT EmptyT) =
-- 	[[x]]
todosLosCaminos (NodeT x ti td) =
	case todosLosCaminos ti ++ todosLosCaminos td of
		[] -> [[x]]
		xss -> agregarAlPrincipio x xss

agregarAlPrincipio :: a -> [[a]] -> [[a]]
agregarAlPrincipio e [] = []
agregarAlPrincipio e (xs:xss) =
	(e:xs) : agregarAlPrincipio e xss



Para todo t. heightT t = length (ramaMasLarga t)

Sea t :: Tree a cualquiera
demostraré por inducción estructural sobre t

Caso base) t = EmptyT -- trivial
Caso ind) t = NodeT x ti td
HI.1) heightT ti = length (ramaMasLarga ti)
HI.2) heightT td = length (ramaMasLarga td)
TI) ¿ heightT (NodeT x ti td) = length (ramaMasLarga (NodeT x ti td)) ?

-- lado izq
heightT (NodeT x ti td)
= -- def heightT
1 + max (heightT ti) (heightT td)
= -- HI.1) HI.2)
1 + max (length (ramaMasLarga ti)) (length (ramaMasLarga td))
= -- def max
1 + if length (ramaMasLarga ti) > length (ramaMasLarga td)
       then length (ramaMasLarga ti)
       else length (ramaMasLarga td)
= -- prop. de función con respecto a un if
if length (ramaMasLarga ti) > length (ramaMasLarga td)
   then 1 + length (ramaMasLarga ti)
   else 1 + length (ramaMasLarga td)

-- lado der
length (ramaMasLarga (NodeT x ti td))
= -- def ramaMasLarga
length (if length (ramaMasLarga ti) > length (ramaMasLarga td)
           then x : ramaMasLarga ti
           else x : ramaMasLarga td)
= -- prop. de función con respecto a un if
if length (ramaMasLarga ti) > length (ramaMasLarga td)
   then length (x : ramaMasLarga ti)
   else length (x : ramaMasLarga td)
= -- def length x2
if length (ramaMasLarga ti) > length (ramaMasLarga td)
   then 1 + length (ramaMasLarga ti)
   else 1 + length (ramaMasLarga td)

-- max x y = if x > y
-- 	         then x
-- 	         else y

-- Tarea:
-- max x y = (-1) * min (-x) (-y)

-- Sugerencia:

-- lema
-- se demuestra por caso
-- tarea
Para todo f, x, y. 
  f (if b then x else y) = if b then f x else f y

-- trivial