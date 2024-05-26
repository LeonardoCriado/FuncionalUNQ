-- data [a] = []
--          | a : [a]

-- f [] = ...
-- f (x:xs) = ... f xs

1) length (xs ++ ys) = length xs + length ys

length [] = 0
length (x:xs) = 1 + length xs

2) elem e (xs ++ ys) = elem e xs || elem e ys

elem e [] = False
elem e (x:xs) = e == x || elem e xs

3) all p (xs ++ ys) = all p xs && all p ys

all p [] = True
all p (x:xs) = p x && all p xs

4) sum (xs ++ ys) = sum xs + sum ys

sum [] = 0
sum (x:xs) = x + sum xs

5) concat (xs ++ ys) = concat xs ++ concat ys

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

6) 
 Si xs /= [] && ys /= []
 entonces maximum (xs ++ ys) = max (maximum xs) (maximum ys)

maximum [] = error "no hay elem"
maximum [x] = x
maximum (x:xs) = max x (maximum xs)

length xs = length (reverse xs)

reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

(++) [] ys = ys
(++) (x:xs) ys = x : (++) xs ys

-- length (reverse xs ++ [x])
-- = -- distribución de length sobre ++
-- length (reverse xs) + length [x]

¿ elem e xs = elem e (reverse xs) ?

-- tarea

snoc e [] = [e]
snoc e (x:xs) = x : snoc e xs

¿ length (e : xs) = length (snoc e xs) ?