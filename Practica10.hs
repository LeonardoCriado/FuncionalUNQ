data NExp = Var Variable
            | NCte Int
            | NBOp NBinOp NExp NExp
            deriving Show 
data NBinOp = Add | Sub | Mul | Div | Mod | Pow deriving Show 
type Variable = String

--f Var v  = ...
--f NCte i = ...
--f NBOp nbo ne1 ne2 = 
--    ... f ne1
--    ... f ne2

type Memoria = [(Variable,Int)] 

enBlanco :: Memoria -- describe una memoria vacía.
enBlanco = []

cuantoVale :: Variable -> Memoria -> Maybe Int -- describe el número asociado a la variable dada en la memoria dada.
cuantoVale s []     = Nothing
cuantoVale s (x:xs) = if esClave s x
                        then Just (snd x)
                        else cuantoVale s xs

esClave :: Eq a => a -> (a, b) -> Bool
esClave s (c, v) = s == c

recordar :: Variable -> Int -> Memoria -> Memoria -- memoria resultante de asociar el número dado a la variable dada en la memoria dada.
recordar v i m  = (v,i) : m

variables :: Memoria -> [Variable] -- describe las variables que la memoria recuerda.
variables []     = []
variables (x:xs) = agregarSiNoExiste (fst x) (variables xs)

agregarSiNoExiste :: Eq a => a -> [a] -> [a]
agregarSiNoExiste v vs = if elemM v vs 
                            then vs
                            else v:vs

elemM :: Eq a => a -> [a] -> Bool
elemM a []     = False
elemM a (x:xs) = a == x || elemM a xs


evalNExp :: NExp -> Memoria -> Int  
--describe el número resultante de evaluar la expresión dada a partir de la memoria dada.
evalNExp (Var v)            m = maybeM2Int (cuantoVale v m)
evalNExp (NCte i)           m = i
evalNExp (NBOp nbo ne1 ne2) m = evalNBOp nbo (evalNExp ne1 m) (evalNExp ne2 m)

evalNBOp :: NBinOp -> Int -> Int -> Int
evalNBOp Add = (+)
evalNBOp Sub = (-)
evalNBOp Mul = (*)
evalNBOp Div = div
evalNBOp Mod = mod
evalNBOp Pow = (^)

maybeM2Int:: Maybe Int -> Int
maybeM2Int Nothing  = error "la variable no está en memoria"
maybeM2Int (Just a) = a

cfNExp :: NExp -> NExp 
-- describe una expresión con el mismo significado que la dada, pero simplificada y reemplazando las
-- subexpresiones que no dependan de la memoria por su expresión más sencilla. La resolución debe ser exclusivamente simbólica.
cfNExp (Var v)            = Var v
cfNExp (NCte i)           = NCte i
cfNExp (NBOp nbo ne1 ne2) = foldNExp nbo (cfNExp ne1) (cfNExp ne2)

foldNExp :: NBinOp -> NExp -> NExp -> NExp
foldNExp nbo (NCte i) (NCte i') = NCte (evalNBOp nbo i i')
foldNExp nbo ne1      ne2       = NBOp nbo ne1 ne2

-- ¿evalNExp . cfNExp = evalNExp?
-- 
-- Por ppio de extensionalidad
-- 
-- Para todo x.
-- (evalNExp . cfNExp) x = evalNExp x
-- 
-- Por def de (.) y Por ppio de extensionalidad
-- 
-- Para todo x. Para todo m
-- ¿ evalNExp (cfNExp x) m = evalNExp x m?
-- 
-- Sea x un elemento de "NExp" y m un elemento de "Memoria". Por principio de induccion en la estructura de x se verifica que:
-- 
-- CB1 x = Var s) siendo s un string cualquiera
-- ¿ evalNExp (cfNExp (Var s)) m = evalNExp (Var s) m ?
-- 
-- CB2 x = NCte i) Siendo i un Int cualquiera
-- ¿ evalNExp (cfNExp (NCte i)) m = evalNExp (NCte i) m ?
-- 
-- CI x = NBOp nbo ne1 ne2 ) Siendo nbo un elemento cualquiera de "NBinOp"
-- HI1) ¡ evalNExp (cfNExp ne1) m = evalNExp ne1 m !
-- HI2) ¡ evalNExp (cfNExp ne2) m = evalNExp ne2 m !
-- TI) ¿ evalNExp (cfNExp (NBOp nbo ne1 ne2)) m = evalNExp (NBOp nbo ne1 ne2) m ?
-- 
-- CB1:
-- IZQ) 
-- evalNExp (cfNExp (Var s)) m
-- =                         (cfNExp)
-- evalNExp (Var s) m 
-- 
-- CB2:
-- IZQ)
-- evalNExp (cfNExp (NCte i)) m
-- =                         (cfNExp)
-- evalNExp (NCte i) m
-- 
-- CI:
-- IZQ)
-- evalNExp (cfNExp (NBOp nbo ne1 ne2)) m
-- =                                                                (cfNExp)
-- evalNExp (fold nbo (cfNExp ne1) (cfNExp ne2)) m
-- =                                                                LEMA: evalNExp (fold nbo x y) m = evalNExp (NBOp nbo x y) m
-- evalNExp (NBOp nbo (cfNExp ne1) (cfNExp ne2)) m
-- =                                                                (evalNExp)    
-- evalNBOp nbo (evalNExp (cfNExp ne1) m) (evalNExp (cfNExp ne2) m)
-- =                                                                HI1 y HI2
-- evalNBOp nbo (evalNExp ne1 m) (evalNExp ne2 m) <----------------------------|
--                                                                             |
-- DER)                                                                        |
-- evalNExp (NBOp nbo ne1 ne2) m                                               | =
-- =                                              (evalNExp)                   |
-- evalNBOp nbo (evalNExp ne1 m) (evalNExp ne2 m) <----------------------------|
-- 
-- Queda entonces demostrada la propiedad.
-- 
-- LEMA:
-- Para todo nbo, Para todo x, Para todo y, Para todo m.
-- ¿ evalNExp (fold nbo x y) m = evalNExp (NBOp nbo x y) m ?
-- 
-- Siendo nbo un elemento cualquier a de "NBinOp", x e y elementos cualqui de "NExp" y m un elemento cualquiera de "Memoria".
-- Se verifica que:
-- 
-- CASO 1: x = NCte i , y = NCte i' )  Siendo i e i' numeros cualquiera.
-- IZQ)
-- evalNExp (fold nbo (NCte i) (NCte i')) m
-- =                                        (fold)
-- evalNExp (NCte (evalNBOp nbo i i')) m
-- =                                        (evalNExp)
-- evalNBOp nbo i i' <-------------------------------------------------|
--                                                                     |
-- DER)                                                                |
-- evalNExp (NBOp nbo (NCte i) (NCte i')) m                            |
-- =                                           (evalNExp)              | =
-- evalNBOp nbo (evalNExp (NCte i) m) (evalNExp (NCte i') m)           |
-- =                                           (evalNExp, evalNExp)    |
-- evalNBOp nbo i i' <-------------------------------------------------|
-- 
-- 
-- CASO 2: x /= NCte i , y /= NCte i' )  Siendo i e i' numeros cualquiera.
-- IZQ)
-- evalNExp (fold nbo x y) m
-- =                          (evalNExp)
-- evalNExp (NBOp nbo x y) m <-------------|
--                                         |
-- DER)                                    | =
-- evalNExp (NBOp nbo x y) m <-------------|
-- 
-- 
-- Queda entonces demostrada la propiedad.


--EJ 2

data BExp = BCte Bool
            | Not BExp
            | And BExp BExp
            | Or BExp BExp
            | ROp RelOp NExp NExp
            deriving Show 

data RelOp = Eq | NEq  -- Equal y NotEqual
            | Gt | GEq -- Greater y GreaterOrEqual
            | Lt | LEq -- Lower y LowerOrEqual
            deriving Show 

-- f (BCte b)           = ...
-- f (Not  be)          = ... f be
-- f (And  be1 be2)     = ... f be1 ... f be2
-- f (Or   be1 be2)     = ... f be1 ... f be2
-- f (ROp  ro  ne1 ne2) = ...


evalBExp :: BExp -> Memoria -> Bool 
-- describe el booleano que resulta de evaluar la expresión dada a partir de la memoria dada.
evalBExp (BCte b)           m = b
evalBExp (Not  be)          m = not (evalBExp be m)
evalBExp (And  be1 be2)     m = evalBExp be1 m && evalBExp be2 m
evalBExp (Or   be1 be2)     m = evalBExp be1 m || evalBExp be2 m
evalBExp (ROp  ro  ne1 ne2) m = evalROp ro (evalNExp ne1 m) (evalNExp ne2 m)

evalROp :: RelOp -> Int -> Int -> Bool
evalROp Eq  = (==)
evalROp NEq = (/=)
evalROp Gt  = (>)
evalROp GEq = (>=)
evalROp Lt  = (<)
evalROp LEq = (<=)


cfBExp :: BExp -> BExp 
-- describe una expresión con el mismo significado que la dada, pero reemplazando las
-- subexpresiones que no dependan de la memoria por su expresión
-- más sencilla. La resolución debe ser exclusivamente simbólica.
cfBExp (BCte b)           = BCte b
cfBExp (Not  be)          = foldNot    (cfBExp be)
cfBExp (And  be1 be2)     = foldAnd    (cfBExp be1) (cfBExp be2)
cfBExp (Or   be1 be2)     = foldOr     (cfBExp be1) (cfBExp be2)
cfBExp (ROp  ro  ne1 ne2) = foldRop ro (cfNExp ne1) (cfNExp ne2)

foldNot :: BExp -> BExp
foldNot (BCte b) = BCte (not b)
foldNot be       = Not be

foldAnd :: BExp -> BExp -> BExp
foldAnd (BCte b1) (BCte b2) = BCte (b1 && b2)
foldAnd be1       be2       = And be1 be2

foldOr :: BExp -> BExp -> BExp
foldOr (BCte b1) (BCte b2) = BCte (b1 || b2)
foldOr be1       be2       = Or be1 be2

foldRop :: RelOp -> NExp -> NExp -> BExp
foldRop ro (NCte i) (NCte i') = BCte (evalROp ro i i')
foldRop ro ne1      ne2       = ROp  ro  ne1 ne2

Demo:
¿ evalBExp . cfBExp = evalBExp ?

por ppio de extensionalidad

¿ (evalBExp . cfBExp) x = evalBExp x ?

por def de (.)

¿ evalBExp (cfBExp x) = evalBExp x ?

por ppio de extensionalidad

¿ evalBExp (cfBExp x) m = evalBExp x m ?

Sea x un elemento de "BExp" cualquiera, m un elemento cualquiera de "Memoria" 
por principio de induccion en la estructura de x. Se verifica que:

Caso Base 1: x = BCte b) siendo b un elemento de "Bool" cualquiera.
¿ evalBExp (cfBExp (BCte b)) m = evalBExp (BCte b) m ?

Caso Inductivo 1: x = Not be) 
HI) ¡ evalBExp (cfBExp be) m = evalBExp be m !
TI) ¿ evalBExp (cfBExp (Not be)) m = evalBExp (Not be) m ?

Caso Inductivo 2: x = And be1 be2)
HI1) ¡ evalBExp (cfBExp be1) m = evalBExp be1 m !
HI2) ¡ evalBExp (cfBExp be2) m = evalBExp be2 m !
TI) ¿ evalBExp (cfBExp (And be1 be2)) m = evalBExp (And be1 be2) m ?

Caso Inductivo 3: x = Or be1 be2)
HI1) ¡ evalBExp (cfBExp be1) m = evalBExp be1 m !
HI2) ¡ evalBExp (cfBExp be2) m = evalBExp be2 m !
TI) ¿ evalBExp (cfBExp (Or be1 be2)) m = evalBExp (Or be1 be2) m ?


Caso Base 2: x = ROp ro ne1 ne2) siendo ro un elemento de "RelOp" cualquiera, ne1 y ne2 elementos cualquiera de "NExp"
¿ evalBExp (cfBExp (ROp ro ne1 ne2)) m = evalBExp (ROp ro ne1 ne2) m ?

-- Inicio ---------------- CB 2 ------------------------------

IZQ)
evalBExp (cfBExp (ROp ro ne1 ne2)) m
=                                                (evalBExp)
evalBExp (foldRop ro (cfNExp ne1) (cfNExp ne2)) m
=                                                LEMA: evalBExp (foldRop ro x y) m = evalBExp (ROp ro x y) m
evalBExp (ROp ro ne1 ne2) m <---------|
                                      |
DER)                                  =
evalBExp (ROp ro ne1 ne2) m <---------|

LEMA:
Para todo ro, para todo x, para todo y, para todo m
¿ evalBExp (foldRop ro x y) m = evalBExp (ROp ro x y) m ?


CASO 1) x = (NCte i) & y = (NCte i')

IZQ)
evalBExp (foldRop ro (NCte i) (NCte i')) m
=                                       (foldRop)
evalBExp (BCte (evalROp ro i i')) m
=                                       (evalBExp)
evalROp ro i i' <--------------------------------------------------------------|
                                                                               |
DER)                                                                           |
evalBExp (ROp ro (NCte i) (NCte i')) m                                         |
=                                       (evalBExp)                             =
evalROp ro (evalNExp (NCte i) m) (evalNExp (NCte i') m)                        |
=                                       (evalNExp y evalNExp)                  |
evalROp ro i i' <--------------------------------------------------------------|

CASO 2) x /= (NCte i) & y /= (NCte i')

IZQ)
evalBExp (foldRop ro x y) m
=                                       (foldRop)
evalBExp (ROp ro x y) m <---------|
                                  |
DER)                              =
evalBExp (ROp ro x y) m <---------|

-- Fin ------------------- CB 2 ------------------------------

-- Inicio ---------------- CB 1 ------------------------------

¿ evalBExp (cfBExp (BCte b)) m = evalBExp (BCte b) m ?

IZQ)
evalBExp (cfBExp (BCte b)) m
=                           (cfBExp) 
evalBExp (BCte b) m <------- = ------> DER) evalBExp (BCte b) m

-- Fin ------------------- CB 1 ------------------------------

-- Inicio ---------------- CI 1 ------------------------------

HI) ¡ evalBExp (cfBExp be) m = evalBExp be m !
TI) ¿ evalBExp (cfBExp (Not be)) m = evalBExp (Not be) m ?

IZQ) 
evalBExp (cfBExp (Not be)) m
=                           (cfBExp)
evalBExp (foldNot (cfBExp be)) m
=                           LEMA: evalBExp (foldNot x) m = evalBExp (Not x) m
evalBExp (Not (cfBExp be)) m
=                           (evalBExp)
not (evalBExp (cfBExp be) m)
=                           HI
not (evalBExp be m) <------------------------|
                                             |
DER)                                         |
evalBExp (Not be) m                          =
=                           (evalBExp)       |
not (evalBExp be m) <------------------------|

LEMA: 
¿ evalBExp (foldNot x) m = evalBExp (Not x) m ?

CASO 1: x = BCte b )
¿ evalBExp (foldNot (BCte b)) m = evalBExp (Not (BCte b)) m ?

IZQ)
evalBExp (foldNot (BCte b)) m
=                           (foldNot)
evalBExp (BCte (not b)) m
=                           (evalBExp)
not b <--------------------------------|
                                       |
DER)                                   |
evalBExp (Not (BCte b)) m              |
=                           (evalBExp) =
not (evalBExp (BCte b) m)              |
=                           (evalBExp) |
not b <--------------------------------|

CASO 2: x /= BCte b )
IZQ)
evalBExp (foldNot x) m
=                           (foldNot)
evalBExp (Not x) m <------- = ------> DER) evalBExp (Not x) m

-- Fin ------------------- CI 1 ------------------------------

-- Inicio ---------------- CI 2 ------------------------------

HI1) ¡ evalBExp (cfBExp be1) m = evalBExp be1 m !
HI2) ¡ evalBExp (cfBExp be2) m = evalBExp be2 m !
TI) ¿ evalBExp (cfBExp (And be1 be2)) m = evalBExp (And be1 be2) m ?

IZQ)
evalBExp (cfBExp (And be1 be2)) m
=                           (cfBExp)
evalBExp (foldAnd (cfBExp be1) (cfBExp be2)) m
=                           LEMA: evalBExp (foldAnd x y) m = evalBExp (And x y) m
evalBExp (And (cfBExp be1) (cfBExp be2)) m
=                           (evalBExp)
evalBExp (cfBExp be1) m && evalBExp (cfBExp be1) m
=                           HI1 y HI2
evalBExp be1 m && evalBExp be1 m <-----|
                                       |
DER)                                   =
evalBExp (And be1 be2)  m              |
=                           (evalBExp) |
evalBExp be1 m && evalBExp be1  m <----|

LEMA: 
Para todo x, Para todo y, para todo m
¿ evalBExp (foldAnd x y) m = evalBExp (And x y) m?

CASO 1: x = (BCte b1) & y = (BCte b2)

IZQ)
evalBExp (foldAnd (BCte b1) (BCte b2)) m
=                           (foldAnd)
evalBExp (BCte (b1 && b2)) m
=                           (evalBExp)
b1 && b2 <-----------------------------------------|
                                                   |
DER)                                               |
evalBExp (And (BCte b1) (BCte b2)) m               |
=                           (evalBExp)             =
evalBExp (BCte b1) m && evalBExp (BCte b2) m       |
=                           (evalBExp, evalBExp)   |
b1 && b2 <-----------------------------------------|


CASO 2: x /= (BCte b1) & y /= (BCte b2)

IZQ)
evalBExp (foldAnd x y) m
=                           (foldAnd) 
evalBExp (And x y) m <------- = ------> DER) evalBExp (And x y) m

-- Fin ------------------- CI 2 ------------------------------

-- Inicio ---------------- CI 3 ------------------------------

HI1) ¡ evalBExp (cfBExp be1) m = evalBExp be1 m !
HI2) ¡ evalBExp (cfBExp be2) m = evalBExp be2 m !
TI) ¿ evalBExp (cfBExp (Or be1 be2)) m = evalBExp (Or be1 be2) m ?

IZQ) 
evalBExp (cfBExp (Or be1 be2)) m
=                           (cfBExp)  
evalBExp (foldOr (cfBExp be1) (cfBExp be2)) m 
=                           LEMA: evalBExp (foldOr x y) m = evalBExp (Or x y) m
evalBExp (Or (cfBExp be1) (cfBExp be2)) m
=                           (evalBExp)  
evalBExp (cfBExp be1) m || evalBExp (cfBExp be2) m 
=                           HI1 y HI2
evalBExp be1 m || evalBExp be2 m <------|
                                        |
DER)                                    | 
evalBExp (Or be1 be2) m                 =
=                           (evalBExp)  |
evalBExp be1 m || evalBExp be2 m <------|

LEMA: 
Para todo x, para todo y, para todo m
evalBExp (foldOr x y) m = evalBExp (Or x y) m

CASO 1: x = (BCte b1) & y = (BCte b2)

IZQ)
evalBExp (foldOr (BCte b1) (BCte b2)) m
=                           (foldOr)
evalBExp (BCte (b1 || b2)) m
=                           (evalBExp)
b1 || b2 <----------------------------------------|
                                                  |
DER)                                              |
evalBExp (Or (BCte b1) (BCte b2)) m               |
=                           (evalBExp)            =
evalBExp (BCte b1) m || evalBExp (BCte b2) m      |
=                           (evalBExp, evalBExp)  |
b1 || b2 <----------------------------------------|

CASO 2: x /= (BCte b1) & y /= (BCte b2)

IZQ)
evalBExp (foldOr x y) m
=                           (foldOr)
evalBExp (Or x y) m <------- = ------> DER) evalBExp (Or x y) m

-- Fin ------------------- CI 3 ------------------------------

Queda entonces demostrada la propiedad.