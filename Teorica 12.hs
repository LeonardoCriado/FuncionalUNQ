data BST23 a =  Cero
              | Dos  Int Int (BST23 a) a (BST23 a)
              | Tres Int Int (BST23 a) a (BST23 a) a (BST23 a)
              deriving (Show, Eq)


fold23:: b -> (Int -> Int -> b -> a -> b -> b) 
           -> (Int -> Int -> b -> a -> b -> a -> b -> b) 
           -> BST23 a 
           -> b
fold23 z f g Cero                       = z
fold23 z f g (Dos h n t1 x t2)          = f h n (fold23 z f g t1) x (fold23 z f g t2)
fold23 z f g (Tres h n t1 x t2 y t3) = g h n (fold23 z f g t1) x (fold23 z f g t2) y (fold23 z f g t3)

rec23:: b -> (Int -> Int -> b -> BST23 a -> a -> b -> BST23 a -> b) 
          -> (Int -> Int -> b -> BST23 a -> a -> b -> BST23 a -> a -> b -> BST23 a -> b) 
          -> BST23 a 
          -> b
rec23 z f g Cero                    = z
rec23 z f g (Dos h n t1 x t2)       = f h n (rec23 z f g t1) t1 x (rec23 z f g t2) t2 
rec23 z f g (Tres h n t1 x t2 y t3) = g h n (rec23 z f g t1) t1 x (rec23 z f g t2) t2 y (rec23   z f g t3) t3


inOrder :: BST23 a -> [a]
inOrder = fold23 [] 
                 (\h -> \n -> \t1 -> \x -> \t2 ->              t1 ++ [x] ++ t2)
                 (\h -> \n -> \t1 -> \x -> \t2 -> \y -> \t3 -> t1 ++ [x] ++ t2 ++ [y] ++ t3)

size23 :: BST23 a -> Int
size23 = fold23 0
                (\h -> \n -> \t1 -> \x -> \t2 ->              1 + t1 + t2)
                (\h -> \n -> \t1 -> \x -> \t2 -> \y -> \t3 -> 2 + t1 + t2 + t3)

height23 ::  BST23 a -> Int
height23 = fold23 0 
                  (\h -> \n -> \t1 -> \x -> \t2 ->              1 + (max t1 t2))
                  (\h -> \n -> \t1 -> \x -> \t2 -> \y -> \t3 -> 2 + max (max t1 t2) t3)


maxElem :: BST23 a -> a
maxElem = rec23 (error "No hay elementos")
                (\h -> \n -> \t1r -> \t1 -> \x -> \t2r -> \t2 ->                      if (esCero t2) then x else t2r)
                (\h -> \n -> \t1r -> \t1 -> \x -> \t2r -> \t2 -> \y -> \t3r -> \t3 -> if (esCero t3) then y else t3r)

esCero Cero = True
esCero _    = False

minElem :: BST23 a -> a
minElem = rec23 (error "No hay elementos")
                (\h -> \n -> \t1r -> \t1 -> \x -> \t2r -> \t2 ->                      if ((not.esCero) t1) then t1r else x )
                (\h -> \n -> \t1r -> \t1 -> \x -> \t2r -> \t2 -> \y -> \t3r -> \t3 -> if ((not.esCero) t1) then t1r else x )


search23 :: Eq a => a -> BST23 a -> Maybe a
search23 e = fold23 Nothing
                    (\h -> \n -> \t1 -> \x -> \t2 ->              elegirJust [ t1 , justIf (==) e x , t2] )
                    (\h -> \n -> \t1 -> \x -> \t2 -> \y -> \t3 -> elegirJust [ t1 , justIf (==) e x , t2 , justIf (==) e y , t3 ] )


elegirJust :: [Maybe a] -> Maybe a
elegirJust = foldr justIfJust Nothing

justIfJust :: Maybe a -> Maybe a -> Maybe a 
justIfJust (Just x) _ = Just x
justIfJust _        y = y

justIf :: (a -> b -> Bool) -> a -> b -> Maybe a 
justIf f x y = if f x y then Just x else Nothing


insert23 :: Eq a => a -> BST23 a -> BST23 a
insert23 e = fold23 (insertarCero e Cero)
                    (\h -> \n -> \t1 -> \x -> \t2 ->            if e==x 
                                                                   then Dos h n t1 x t2 
                                                                   else insertarDos h n t1 x t2 e )
                    (\h -> \n -> \t1 -> \x -> \t2 -> \y -> \t3 -> elegirJust [ t1 , justIf (==) e x , t2 , justIf (==) e y , t3 ] )


insertarCero :: a -> BST23 a -> BST23 a
insertarCero e = Dos 1 1 Cero e

insertarDos :: Int -> Int -> BST23 a -> a -> BST23 a -> a -> BST23 a
insertarDos h n t1 x t2 e
            | e > (maxElem t1) && e < (minElem t2) = 


e1 = Tres 3 5 
      (Dos 2 1 Cero 1 Cero) 
      2 
      (Dos 2 1 Cero 3 Cero) 
      4
      (Dos 2 1 Cero 5 Cero)


e2 = Cero                 

