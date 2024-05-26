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

Para todo e. Para todo xs
¿ elem e xs = elem e (reverse xs) ?
-- tarea
Siendo xs una lista cualquiera y e un elemento cualquiera. Por induccion sobre la estructura de xs se ve:

Caso base:  xs = [] )
¿ elem e [] = elem e (reverse []) ?
IZQ)
elem e []

DER)
elem e (reverse [])
=           (reverse)
elem e []


Caso Inductivo: xs = x:xs')
HI) ¡ elem e xs' = elem e (reverse xs') ! 
TI) elem e (x:xs') = elem e (reverse (x:xs'))

IZQ)
elem e (x:xs')
=                                            (elem)
e == x || elem e xs'
=                                             HI
e == x || elem e (reverse xs')   <--------------------------------------------------|       
                                                                                    |
DER)                                                                                |
elem e (reverse (x:xs'))                                                            |
=                                            (reverse)                              |
elem e (reverse xs' ++ [x])                                                         |
=                                  LEMA: elem e (xs ++ ys) = elem e xs || elem e ys |
elem e (reverse xs') || elem e [x]                                                  |
=                                            (conm ||)                              | queda demostrado
elem e [x] || elem e (reverse xs')                                                  |
=                                            (elem) 2 veces                         |
e == x || False || elem e (reverse xs')                                             |
=                                            (conm ||)                              |
False || e == x || elem e (reverse xs')                                             |
=                                            (Identidad ||)                         |
e == x || elem e (reverse xs')   <--------------------------------------------------|


LEMA:
Para todo xs, Para todo ys, Para todo e. 
¿ elem e (xs ++ ys) = elem e xs || elem e ys? 

Sea xs una lista cualquiera, ys otra lista cualquiera y e un elemento cualquiera del mismo tipo que los elementos de la lista.
Por inducion sobre la estructura de xs, se ve:

Caso base: xs [])
¿ elem e ([] ++ ys) = elem e [] || elem e ys? 
 
IZQ)
elem e ([] ++ ys)
=                         (++)
elem e ys <--------------------------------------------------|  
                                                             |
DER)                                                         |
elem e [] || elem e ys                                       | 
=                         (elem)                             | queda demostrado
False || elem e ys                                           |
=                         (Identidad ||)                     |
elem e ys <--------------------------------------------------|  


Caso inductivo: xs = x:xs')
HI) ¡ elem e (xs' ++ ys) = elem e xs' || elem e ys !
TI) ¿ elem e ((x:xs') ++ ys) = elem e (x:xs') || elem e ys? 

IZQ)
elem e ((x:xs') ++ ys)
=                               (++)
elem e (x : (xs' ++ ys))
=                               elem
e == x || elem e (xs' ++ ys)
=                               HI
e == x || elem e xs' || elem e ys  <-------------------------------------------|
                                                                               |
DER)                                                                           |
elem e (x:xs') || elem e ys                                                    | queda demostrado
=                           (elem)                                             |
e == x || elem e xs' || elem e ys  <-------------------------------------------|




snoc e [] = [e]
snoc e (x:xs) = x : snoc e xs

Para todo e. Para todo xs.
¿ length (e : xs) = length (snoc e xs) ?

Sea xs una lista cualquiera y e un elemento del mismo tipo que la lista de xs. 
Por induccion en la estructura de xs se ve que:

Caso base: xs = [])
¿ length (e : []) = length (snoc e []) ?

Caso inductivo: xs = x:xs')
HI) ¡ length (e : xs') = length (snoc e xs') !
Por definicion de length:
    ¡ 1 + length xs' = length (snoc e xs') !
TI) ¿ length (e : (x:xs')) = length (snoc e (x:xs')) ?

IZQ)
length (e : (x:xs'))
=                       (length)
1 + length (x:xs')
=                       (length)
1 + 1 + length (xs')
=                       HI
1 + length (snoc e xs') <-------------------------------------------|
                                                                    |
                                                                    |
DER)                                                                |
length (snoc e (x:xs'))                                             | queda demostrado
=                       (snoc)                                      |
length (x : snoc e xs')                                             |
=                       (length)                                    |
1 + length (snoc e xs') <-------------------------------------------|
