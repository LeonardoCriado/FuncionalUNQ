data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

foldT:: b -> (a -> b -> b -> b) -> Tree a -> b
foldT e n EmptyT          = e
foldT e n (NodeT a t1 t2) = n a (foldT e n t1) (foldT e n t2)

mapT :: (a -> b) -> Tree a -> Tree b
mapT f = foldT EmptyT (NodeT . f)

sumT :: Tree Int -> Int
sumT = foldT 0 (\e t1 t2 -> e + t1 + t2)

sizeT :: Tree a -> Int
sizeT = foldT 0 (\e t1 t2 -> 1 + t1 + t2)

heightT :: Tree a -> Int
heightT = foldT 1 (\e t1 t2 -> 1 + (t1 `max` t2))

preOrder :: Tree a -> [a]
preOrder = foldT [] (\e t1 t2 -> e : (t1 ++ t2))

inOrder :: Ord a => Tree a -> [a]
inOrder = foldT [] (\e t1 t2 -> insertOrd' e (t1 ++ t2))

insertOrd' :: Ord a => a -> [a] -> [a]
insertOrd' a = recr (\x xr xs -> if a <= x then a : xs else x : xr) [a]

recr :: (a -> b -> [a] -> b) -> b -> [a] -> b
recr f b []     = b
recr f b (x:xs) = f x (recr f b xs) (x:xs) 

postOrder :: Tree a -> [a]
postOrder = foldT [] (\e t1 t2 -> (t1 ++ t2) ++ [e] )

mirrorT :: Tree a -> Tree a
mirrorT = foldT EmptyT (\e t1 t2 -> NodeT e t2 t1)

countByT :: (a -> Bool) -> Tree a -> Int
countByT p = foldT 0 (\e t1 t2 -> bool2int (p e) + t1 + t2)

bool2int True  = 1
bool2int False = 0

partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
partitionT p = foldT ([],[]) (\e t1 t2 -> consIf (p e) e (unirTuplaListas t1 t2))

consIf :: Bool -> a -> ([a], [a]) -> ([a], [a])
consIf True  x (xs, ys) = (x:xs, ys  )
consIf False x (xs, ys) = (xs  , x:ys)

unirTuplaListas :: ([a], [a]) -> ([a], [a]) -> ([a], [a])
unirTuplaListas (xs, ys) (xs', ys') = (xs ++ xs' , ys ++ ys')

zipWithT' :: (a->b->c) -> Tree a -> Tree b -> Tree c
zipWithT' f (NodeT x t1 t2) (NodeT y t1' t2') = NodeT (f x y) (zipWithT' f t1 t1') (zipWithT' f t2 t2')
zipWithT' f t               t'                = EmptyT 

zipWithT :: (a->b->c) -> Tree a -> Tree b -> Tree c
zipWithT f = foldT (\tb -> EmptyT) (\x t1 t2 -> \tb -> case tb of
                                                            EmptyT          -> EmptyT
                                                            NodeT y tb1 tb2 -> NodeT (f x y) (t1 tb1) (t2 tb2) )

-- zipWithT2 :: (a->b->c) -> Tree a -> Tree b -> Tree c
-- zipWithT2 f = foldT (\tb -> EmptyT) (\x t1 t2 -> \tb -> foldT EmptyT (\y tb1 tb2 -> NodeT (f x y) (t1 tb1) (t2 tb2)))

caminoMasLargo :: Tree a -> [a]
caminoMasLargo = foldT [] (\x t1 t2 -> x : (listaMasLarga t1 t2))

listaMasLarga t1 t2 = if length t1 > length t2 then t1 else t2

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos = foldT [] (\x t1 t2 -> case t1 ++ t2 of 
                                             [] -> [[x]]
                                             _  -> concatATodos x (t1 ++ t2))

concatATodos e = foldr (\x xs -> (e : x) : xs ) []

todosLosNiveles :: Tree a -> [[a]]
todosLosNiveles = foldT [] (\x t1 t2 -> [x] : (zipL t1 t2))

zipL :: [[a]] -> [[a]] -> [[a]]
zipL xss      []       = xss
zipL []       yss      = yss
zipL (xs:xss) (ys:yss) = (xs ++ ys) : (zipL xss yss)


nivelN :: Tree a -> Int -> [a]
nivelN = foldT (\i -> []) (\x t1 t2 -> \i -> if i == 0 
                                                then [x]
                                                else t1 (i-1) ++ t2 (i-1))



recT:: b -> (a -> b -> Tree a -> b -> Tree a -> b) -> Tree a -> b
recT e n EmptyT          = e
recT e n (NodeT a t1 t2) = n a (recT e n t1) t1 (recT e n t2) t2

insertT :: Ord a => a -> Tree a -> Tree a
insertT e = recT (NodeT e EmptyT EmptyT) (\x tr1 t1 tr2 t2 -> case compare e x of
                                                                    EQ -> NodeT x t1  t2 
                                                                    GT -> NodeT x t1  tr2
                                                                    LT -> NodeT x tr1 t2 )

caminoHasta :: Eq a => a -> Tree a -> [a]
caminoHasta e = recT [] (\x tr1 t1 tr2 t2 -> if e == x
                                                then [x]
                                                else if e `existeEn` t1
                                                        then x : tr1
                                                        else x : tr2 )

existeEn :: Eq a => a -> Tree a -> Bool
existeEn e = foldT False (\x t1 t2 -> e == x || t1 || t2)

e1 :: Tree Int
e1 = (NodeT 1 
            EmptyT 
            (NodeT 4
                  EmptyT 
                  (NodeT 3 
                         (NodeT 1 EmptyT (NodeT 3 EmptyT EmptyT)) 
                         (NodeT 1 EmptyT (NodeT 3 EmptyT EmptyT))
                  ) 
            )
      )

e2 :: Tree Int
e2 = (NodeT 5 
            (NodeT 1 
                   EmptyT 
                   EmptyT 
            ) 
            (NodeT 7 
                   EmptyT 
                   (NodeT 8
                          EmptyT 
                          EmptyT
                    )
            )
    )

int2str 1 = "uno"
int2str 2 = "dos"
int2str 3 = "tres"
int2str 4 = "cuatro"
int2str 5 = "cinco"


DEMO: 

para todo f. 
¿ sizeT . mapT f = sizeT ?

por ppio de ext

para todo f. para todo t.
¿ (sizeT . mapT f) t = sizeT t ?

por def de (.)

para todo f. para todo t.
¿ sizeT (mapT f t) = sizeT t ?

Siendo f una funcion cualquiera y t un elemento de "Tree a". 
Por principio de inducción en la estructura de t :

CB) t = EmptyT
¿ sizeT (mapT f EmptyT) = sizeT EmptyT ?

CI) t = NodeT a t1 t2
HI 1) ¡ sizeT (mapT f t1) = sizeT t1 !
HI 2) ¡ sizeT (mapT f t2) = sizeT t2 !
TI) ¿ sizeT (mapT f (NodeT a t1 t2)) = sizeT (NodeT a t1 t2) ?

CB:
IZQ)
  sizeT (mapT f EmptyT)
=                                           (mapT f)
  sizeT (foldT EmptyT (NodeT . f) EmptyT)
=                                           (foldT)
  sizeT EmptyT --> = a derecha

CI:
IZQ)
  sizeT (mapT f (NodeT a t1 t2))
=                                                                                     LEMA : sizeT (mapT f (NodeT a t1 t2)) = 1 + sizeT (mapT f t1) + sizeT (mapT f t2)
  1 + sizeT (mapT f t1) + sizeT (mapT f t2)
=                                                                                     (HI 1 y 2)
  1 + sizeT t1 + sizeT t2
=                                                                                     (sizeT, size T)
  1 + (foldT 0 (\e t1 t2 -> 1 + t1 + t2) t1) + (foldT 0 (\e t1 t2 -> 1 + t1 + t2) t2)

DER)
  sizeT (NodeT a t1 t2)
=                                                                                     (sizeT)
  foldT 0 (\e t1 t2 -> 1 + t1 + t2) (NodeT a t1 t2)
=                                                                                     (foldT)
  (\e t1 t2 -> 1 + t1 + t2) a (foldT 0 (\e t1 t2 -> 1 + t1 + t2) t1) 
                              (foldT 0 (\e t1 t2 -> 1 + t1 + t2) t2)
=                                                                                     (lamda)
  1 + (foldT 0 (\e t1 t2 -> 1 + t1 + t2) t1) + (foldT 0 (\e t1 t2 -> 1 + t1 + t2) t2)



LEMA : 

Para todo f, para todo t1, para todo t2.
¿ sizeT (mapT f (NodeT a t1 t2)) = 1 + sizeT (mapT f t1) + sizeT (mapT f t2) ?

IZQ)
  sizeT (mapT f (NodeT a t1 t2))
=                                                                           (mapT f)
  sizeT (foldT EmptyT (NodeT . f) (NodeT a t1 t2))
=                                                                           (foldT)
  sizeT ((NodeT . f) a (foldT EmptyT (NodeT . f) t1) 
                       (foldT EmptyT (NodeT . f) t2))
=                                                                           (mapT)
  sizeT ((NodeT . f) a (mapT f t1) (mapT f t2))
=                                                                           (.)
  sizeT (NodeT (f a) (mapT f t1) (mapT f t2))
=                                                                           (sizeT)
  foldT 0 (\e t1 t2 -> 1 + t1 + t2) (NodeT (f a) (mapT f t1) 
                                                 (mapT f t2)) 
=                                                                           (foldT)
  (\e t1 t2 -> 1 + t1 + t2) (f a) 
                            (foldT 0 (\e t1 t2 -> 1 + t1 + t2) (mapT f t1))  
                            (foldT 0 (\e t1 t2 -> 1 + t1 + t2) (mapT f t2))
=                                                                           (mapT)
  1 + (foldT 0 (\e t1 t2 -> 1 + t1 + t2) (mapT f t1)) + 
      (foldT 0 (\e t1 t2 -> 1 + t1 + t2) (mapT f t2))
=                                                                           (sizeT)
  1 + (sizeT (mapT f t1)) + (sizeT (mapT f t2))




ii. para todo f. para todo g. mapT f . mapT g = mapT (f . g)
iii. foldT EmptyT NodeT = id