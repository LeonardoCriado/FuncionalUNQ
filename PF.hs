
suma:: Int -> Int -> Int
suma = \x -> \y -> x + y

compose :: (a -> b) -> (c -> a) -> c -> b
compose f g x = f(g x)

twice:: (a->a) -> a -> a
twice = \f -> \x -> f(f x)

many :: Int -> (a->a) -> a -> a
many 0 f x = f x
many n f x = f ( many (n-1) f x)

subset :: Eq a => [a] -> [a] -> Bool
subset = foldr (\x xs ys -> elem x ys && xs ys) (const True)

accumsum :: [Int] -> [Int]
accumsum = recr (\x xr xs -> (x + sum xs) : xr) []

recr :: (a -> b -> [a] -> b) -> b -> [a] -> b
recr f g []     = g
recr f g (x:xs) = f x (recr f g xs) xs

