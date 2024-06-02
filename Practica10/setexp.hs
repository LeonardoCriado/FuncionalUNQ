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
evalS1 Empty
	          | Add a (SetExp a)
              | Remove a (SetExp a)
              | Union (SetExp a) (SetExp a)
              | Intersection (SetExp a) (SetExp a)

-- como funcion
evalS2 :: Eq a => SetExp a -> (a -> Bool)
evalS2 = undefined

-- como BST
evalS3 :: Ord a => SetExp a -> BST a
evalS3 = undefined

sequenceS :: [SetExp a -> SetExp a] -> (SetExp a -> SetExp a)
sequenceS [] = id
sequenceS (f:fs) = f . sequenceS fs