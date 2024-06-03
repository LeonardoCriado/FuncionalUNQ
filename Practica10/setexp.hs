data SetExp a = Empty
	          | Add a (SetExp a)
              | Remove a (SetExp a)
              | Union (SetExp a) (SetExp a)
              | Intersection (SetExp a) (SetExp a)
              deriving Show

data BST a = EmptyT 
           | NodeT a (BST a) (BST a)
           deriving Show

-- esquema SetExp
-- f Empty                = ...
-- f (Add    a s1)        = ... f s1
-- f (Remove a s1)        = ... f s1
-- f (Union        s2 s2) = ... f s1 ... fs2 
-- f (Intersection s1 s2) = ... f s1 ... fs2 

-- Tarea:
-- a lista sin repetidos
evalS1 :: Eq a => SetExp a -> [a]
evalS1 Empty                = []
evalS1 (Add    a s1)        = evalAdd    a  (evalS1 s1)
evalS1 (Remove a s1)        = evalRemove a  (evalS1 s1)
evalS1 (Union        s1 s2) = evalUnion     (evalS1 s1) (evalS1 s2)
evalS1 (Intersection s1 s2) = evalIntersect (evalS1 s1) (evalS1 s2) 

evalAdd:: Eq a => a -> [a] -> [a]
evalAdd = aplicarSi elem (flip const) (:)

evalRemove:: Eq a => a -> [a] -> [a]
evalRemove = aplicarSi elem dropE (flip const)

evalUnion::Eq a => [a] -> [a] -> [a]
evalUnion []     ys = ys
evalUnion (x:xs) ys = evalAdd x (evalUnion xs ys) 

evalIntersect::Eq a => [a] -> [a] -> [a]
evalIntersect []     ys = []
evalIntersect (x:xs) ys = consSiPertenece x ys (evalIntersect xs ys) 

consSiPertenece:: Eq a => a -> [a] -> [a] -> [a]
consSiPertenece a xs ys = if elem a xs then a:ys else ys

dropE:: Eq a => a -> [a] -> [a]
dropE e []     = []
dropE e (x:xs) = if e == x then xs else (dropE e xs)

aplicarSi:: (a -> b -> Bool) -> (a -> b -> c) -> (a -> b -> c) -> a -> b -> c
aplicarSi fb f g a b = if fb a b
                          then f a b
                          else g a b

e1 = (Add 1 (Add 2 Empty))
e1' = sequenceS [Add 1, Add 2] Empty
e2 = (Add 2 (Add 3 Empty))
e3 = Union e1 e2
e4 = Intersection e1 e2


-- como funcion
evalS2 :: Eq a => SetExp a -> (a -> Bool)
evalS2 Empty                = const False
evalS2 (Add    a s1)        = evalAdd2    a  (evalS2 s1)
evalS2 (Remove a s1)        = evalRemove2 a  (evalS2 s1)
evalS2 (Union        s1 s2) = evalUnion2     (evalS2 s1) (evalS2 s2)
evalS2 (Intersection s1 s2) = evalIntersect2 (evalS2 s1) (evalS2 s2) 

evalAdd2:: Eq a => a -> (a -> Bool) -> (a -> Bool)
evalAdd2 x = componerCon (||) (==x)

evalRemove2:: Eq a => a -> (a -> Bool) -> (a -> Bool)
evalRemove2 x = componerCon (&&) (/=x)

evalUnion2:: Eq a => (a -> Bool) -> (a -> Bool) -> (a -> Bool)
evalUnion2 = componerCon (||)

evalIntersect2:: Eq a => (a -> Bool) -> (a -> Bool) -> (a -> Bool)
evalIntersect2 = componerCon (&&)

componerCon:: (b -> b -> b) -> (a -> b) -> (a -> b) -> a -> b
componerCon f g h x = f (g x) (h x)


-- como BST
evalS3 :: Ord a => SetExp a -> BST a
evalS3 Empty                = EmptyT
evalS3 (Add    a s1)        = evalAdd3    a  (evalS3 s1)
evalS3 (Remove a s1)        = evalRemove3 a  (evalS3 s1)
evalS3 (Union        s1 s2) = evalUnion3     (evalS3 s1) (evalS3 s2)
evalS3 (Intersection s1 s2) = evalIntersect3 (evalS3 s1) (evalS3 s2)

evalAdd3:: Ord a => a -> BST a -> BST a
evalAdd3 = aplicarSi estaEnBST (flip const) insertBST

evalRemove3:: Ord a => a -> BST a -> BST a
evalRemove3 = aplicarSi estaEnBST removeBST (flip const) 


estaEnBST :: (Ord a) => a -> BST a -> Bool
estaEnBST x EmptyT        = False
estaEnBST x (NodeT v l r) = x == v || aplicarARamas x v (estaEnBST x) l r

aplicarARamas :: (Ord a) => a -> a -> (b -> c) -> b -> b -> c
aplicarARamas x v f l r = if x < v then f l else f r

insertBST :: (Ord a) => a -> BST a -> BST a
insertBST x EmptyT        = NodeT x EmptyT EmptyT
insertBST x (NodeT v l r) = if x < v 
                              then NodeT v (insertBST x l) r
                              else NodeT v l (insertBST x r) 

removeBST :: (Ord a) => a -> BST a -> BST a
removeBST x EmptyT        = EmptyT
removeBST x (NodeT v l r) = if x == v 
                              then unirBST l r
                              else if x < v 
                                     then Node v (removeBST x l) r 
                                     else Node v l (removeBST x l)







sequenceS :: [SetExp a -> SetExp a] -> (SetExp a -> SetExp a)
sequenceS [] = id
sequenceS (f:fs) = f . sequenceS fs



--evalRemove2:: Eq a => a -> (a -> Bool) -> (a -> Bool)
--evalRemove2 = aplicarSi (flip $) ((.) not) (flip const) 