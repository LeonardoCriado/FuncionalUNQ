import Prelude hiding (Left,Right)

data Dir = Left | Right | Straight deriving Show
data Mapa a = Cofre [a]
             | Nada (Mapa a)
             | Bifurcacion [a] (Mapa a) (Mapa a) deriving Show

foldM :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b
foldM fc fn fb (Cofre xs)             = fc xs
foldM fc fn fb (Nada m)               = fn (foldM fc fn fb m)
foldM fc fn fb (Bifurcacion xs m1 m2) = fb xs (foldM fc fn fb m1) (foldM fc fn fb m2)

recM :: ([a] -> b) -> (b -> Mapa a -> b) -> ([a] -> b -> Mapa a -> b -> Mapa a -> b) -> Mapa a -> b
recM fc fn fb (Cofre xs)             = fc xs
recM fc fn fb (Nada m)               = fn (recM fc fn fb m) m
recM fc fn fb (Bifurcacion xs m1 m2) = fb xs (recM fc fn fb m1) m1 (recM fc fn fb m2) m2

objects :: Mapa a -> [a]
objects = foldM id id (\xs m1 m2 -> xs ++ m1 ++ m2)

mapM :: (a -> b) -> Mapa a -> Mapa b
mapM f = foldM (Cofre . (map f)) 
               Nada 
               (Bifurcacion . (map f))

has :: (a -> Bool) -> Mapa a -> Bool
has f = foldM (any f) 
              id 
              (\xs m1 m2 -> any f xs && m1 && m2)

hasObjectAt :: (a->Bool) -> Mapa a -> [Dir] -> Bool
-- PRECOND: la lista de direcciones es un camino valido
hasObjectAt f m ds = any f (objectsAt m ds)

objectsAt :: Mapa a -> [Dir] -> [a]
-- PRECOND: la lista de direcciones es un camino valido
objectsAt = foldM f g h
          where f xs       []     = xs
                f xs       _      = error "Camino incorrecto"
                g m        []     = []
                g m        (x:xs) = case x of Straight -> m xs 
                                              _        -> error "Camino incorrecto"
                h ys m1 m2 []     = ys
                h ys m1 m2 (x:xs) = case x of Left  -> m1 xs
                                              Right -> m2 xs
                                              _     -> error "Camino incorrecto"

longestPath :: Mapa a -> [Dir]
longestPath = foldM f g h
            where f _        = []
                  g m        = Straight : m
                  h xs m1 m2 = if length m1 > length m2 then Left : m1 else Right : m2

objectsOfLongestPath :: Mapa a -> [a]
objectsOfLongestPath m = objectsAt m (longestPath m)

allPaths :: Mapa a -> [[Dir]]
allPaths = foldM f g h
         where f _       = [[]]
               g m       = map (Straight:) m ++ [[]]
               h _ m1 m2 = map (Left:) m1 ++ map (Right:) m2 ++ [[]]

objectsPerLevel :: Mapa a -> [[a]]
objectsPerLevel = foldM f g h
                where f xs       = [xs]
                      g m        = [] : m
                      h xs m1 m2 = xs : (combinar' m1 m2) 

combinar :: [[a]] -> [[a]] -> [[a]]
combinar xs     []     = xs
combinar []     ys     = ys
combinar (x:xs) (y:ys) = (x ++ y) : combinar xs ys

combinar' :: [[a]] -> [[a]] -> [[a]]
combinar' = foldr f g
          where f x xs []     = x : xs []
                f x xs (y:ys) = (x ++ y) : xs ys
                g      ys     = ys


e1 = Bifurcacion [1,2]  (Bifurcacion [1] (Cofre []) 
                                         (Nada        (Nada     (Cofre [2])))) 
                        (Nada            (Cofre [1]))

