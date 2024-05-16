import Prelude hiding ( sum, 
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

--Seccion 2
--Ej 1

data N = Z | S N
         deriving Show 

-- f Z     = ...
-- f (S n) = ... f n

evalN :: N -> Int
-- describe el número representado por el elemento dado.
evalN Z     = 0
evalN (S n) = 1 + (evalN n)

addN :: N -> N -> N
-- describe la representación unaria de la suma de los números representados por los argumentos. La
-- resolución debe ser exclusivamente simbólica, o sea, SIN calcular cuáles son esos números.
addN Z      n = n
addN (S n') n = S (addN n' n)

prodN :: N -> N -> N
-- describe la representación unaria del producto de los números representados por los argumentos. La
-- resolución debe ser exclusivamente simbólica.
prodN Z      n = Z
prodN (S n') n = addN n (prodN n' n)

int2N :: Int -> N
--describe la representación unaria del número dado usando el tipo N
int2N 0 = Z
int2N n = S (int2N (n-1))


--Ej 2

type NU = [()]

evalNU :: NU -> Int
-- describe el número representado por el elemento dado.
evalNU []     = 0
evalNU (x:xs) = 1 + evalNU xs

succNU :: NU -> NU
-- describe la representación unaria del resultado de sumarle uno al número representado por el argumento.
-- La resolución debe ser exclusivamente simbólica.
succNU [] = [()]
succNU nu = () : nu

addNU :: NU -> NU -> NU
-- describe la representación unaria de la suma de los números representados por los argumentos. 
-- La resolución debe ser exclusivamente simbólica.
addNU [] nu' = nu'
addNU nu []  = nu
addNU nu nu' = nu ++ nu

nu2n :: NU -> N
-- describe la representación unaria dada por el tipo N correspondiente al número representado por el argumento.
nu2n []     = Z
nu2n (x:xs) = S (nu2n xs)

n2nu :: N -> NU
-- describe la representación unaria dada por el tipo NU correspondiente al número representado por el argumento.
n2nu Z     = []
n2nu (S n) = () : n2nu n


--EJ 3

data DigBin = I | O deriving Show
type NBin = [DigBin]

--0 = 0   = 0
--1 = 1   = 1
--2 = 10  = 01
--3 = 11  = 11
--4 = 100 = 001
--5 = 101 = 101
--6 = 110 = 011
--7 = 111 = 111




evalNB :: NBin -> Int
-- describe el número representado por el elemento dado.
evalNB nb = evalNB' (reverse nb)

evalNB' :: NBin -> Int
evalNB' []       = 0
evalNB' (nb:nbs) = case nb of 
                    I -> 2 ^ length nbs + evalNB' nbs
                    O -> evalNB' nbs


normalizarNB :: NBin -> NBin
-- describe la representación binaria del número representado por el argumento, pero sin “ceros a
-- la izquierda” (dígitos redundantes). OBSERVACIÓN: por la forma de la representación, los “ceros a
-- izquierda” aparecen a la derecha de la lista. Entonces la propiedad
-- indica que una lista de dígitos normalizada no puede terminar con eldígito 0.
normalizarNB []       =  [] 
normalizarNB (nb:nbs) =  if sonTodosO (normalizarNB nbs)
                           then [nb]
                           else nb : normalizarNB nbs

sonTodosO :: NBin -> Bool
sonTodosO []       = True
sonTodosO (nb:nbs) = esO nb && sonTodosO nbs

esO :: DigBin -> Bool
esO O = True
esO _ = False




succNB :: NBin -> NBin
-- describe la representación binaria normalizada del resultado de sumarle uno al número representado
-- por el argumento. La resolución debe ser exclusivamente simbólica, y
-- no debe utilizar normalizarNB. Se puede suponer como precondición que el argumento está normalizado.
succNB = undefined

addNB :: NBin -> NBin -> NBin
-- describe la representación binaria normalizada de la suma de los números
-- representados por los argumentos. La resolución debe ser
-- exclusivamente simbólica (o sea, no usar ninguna forma de eval), y
-- no debe utilizar normalizarNB. Se puede suponer como
-- precondición que los argumentos están normalizados.
-- AYUDA: considerar dos operaciones auxiliares
-- addNBConCarry :: NBin -> NBin -> DigBin -> NBin
-- addDBConCarry :: DigBin -> DigBin -> DigBin -> (DigBin, DigBin)
addNB = undefined



nb2n :: NBin -> N
-- describe la representación unaria dada por el tipo N correspondiente al número representado por el argumento.
nb2n = undefined

n2nb :: N -> NBin
-- describe la representación binaria normalizada dada por el tipo NBin correspondiente al número
-- representado por el argumento
n2nb = undefined