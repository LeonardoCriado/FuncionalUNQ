import Prelude hiding (length, 
                        sum, 
                        product, 
                        concat, 
                        elem, 
                        all, 
                        (++),
                        reverse,
                        zip,
                        unzip,
                        any)

--Ej 1

l :: [a] -> Int
-- describe la cantidad de elementos de la lista.
l []     = 0
l (x:xs) = 1 + l xs

sum :: [Int] -> Int
--describe la suma de todos los elementos de la lista.
sum []     = 0
sum (x:xs) = x + sum xs

product :: [Int] -> Int
-- describe el producto entre todos los elementos de la lista.
product []     = 0
product (x:xs) = x + product xs

concat :: [[a]] -> [a]
--- describe la lista resultante de concatenar todas las listas que son elementos de la dada.
concat []     = []
concat (xs:xss) = xs ++ concat xss

elem :: Eq a => a -> [a] -> Bool
--indica si el elemento dado pertenece a la lista.
elem e []     = False
elem e (x:xs) = e == x || elem e xs

all :: (a -> Bool) -> [a] -> Bool
-- indica si todos los elementos de la lista cumplen el predicado dado.
all p []     = True
all p (x:xs) = p x && all p xs

any :: (a -> Bool) -> [a] -> Bool
-- indica si algún elemento de la lista cumple el predicado dado.
any p []     = False
any p (x:xs) = p x || any p xs

count :: (a -> Bool) -> [a] -> Int
-- describe la cantidad de elementos de la lista que cumplen el predicado dado.
count p []     = 0
count p (x:xs) = unoSiTrue (p x) + count p xs

unoSiTrue:: Bool -> Int
unoSiTrue True = 1
unoSiTrue _    = 0

subset :: Eq a => [a] -> [a] -> Bool
-- indica si todos los elementos de la primera lista se encuentran en la segunda.
subset []     s = True
subset (x:xs) s = elem x s && subset xs s

(++) :: [a] -> [a] -> [a]
-- describe el resultado de agregar los elementos de la primera lista adelante de los elementos de la segunda.
(++) []     ys = ys
(++) (x:xs) ys = x : (++) xs ys

reverse :: [a] -> [a]
-- describe la lista que tiene los elementos en el orden inverso a la lista dada.
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]


zip :: [a] -> [b] -> [(a,b)]
-- describe la lista resultante de juntar de a pares los elementos de ambas listas, según la posición que comparten en cada una
zip []     b      = []
zip a      []     = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

unzip :: [(a,b)] -> ([a],[b])
-- describe el par de listas que resulta de desarmar la lista dada; la primera componente del resultado se
-- corresponde con las primeras componentes de los pares dados, y la segunda componente con las segundas componentes de dichos pares.
unzip []     = ([],[])
unzip (p:ps) = addParAParDeListas p (unzip ps)

addParAParDeListas :: (a,b) -> ([a],[b]) -> ([a],[b])
addParAParDeListas (a,b) (as,bs) = (a:as,b:bs) 

