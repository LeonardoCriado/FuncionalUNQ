
suma:: Int -> Int -> Int
suma = \x -> \y -> x + y

--compose :: (a -> b) -> (c -> a) -> c -> b
compose f g x = f(g x)


