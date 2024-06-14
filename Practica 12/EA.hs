data EA    = Const Int | BOp BinOp EA EA deriving Show
data BinOp = Sum | Mul deriving Show

data ExpA =  Cte Int | Suma ExpA ExpA | Prod ExpA ExpA deriving (Show , Eq)

data ABTree a b = Empty b | NodeT a (ABTree a b) (ABTree a b) deriving Show

foldEA :: (Int -> b) -> (BinOp -> b -> b -> b) -> EA -> b
foldEA c b (Const i)    = c i
foldEA c b (BOp bo x y) = b bo (foldEA c b x) (foldEA c b y)


noTieneNegativosExplicitosEA :: EA -> Bool
noTieneNegativosExplicitosEA = foldEA (>=0) (\bo -> (&&)) 

simplificarEA' :: EA -> EA
---- Describe una expresión con el mismo significado que la dada, pero que no tiene sumas del
---- número 0 ni multiplicaciones por 1 o por 0. La resolución debe ser exclusivamente simbólica.
simplificarEA' = foldEA Const simpBop

simpBop:: BinOp -> EA -> EA -> EA
simpBop Sum (Const 0) y         = y   
simpBop Sum x         (Const 0) = x   
simpBop Sum x         y         = BOp Sum x y
simpBop Mul (Const 0) y         = Const 0 
simpBop Mul x         (Const 0) = Const 0 
simpBop Mul (Const 1) y         = y
simpBop Mul x         (Const 1) = x 
simpBop Mul x         y         = BOp Mul x y

evalEA' :: EA -> Int
-- Describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalEA' = foldEA id evalBop

evalBop :: BinOp -> Int -> Int -> Int
evalBop Sum = (+)
evalBop Mul = (*)


showEA :: EA -> String
---- Describe el string sin espacios y con paréntesis correspondiente a la expresión dada.
showEA = foldEA show showBOp

showBOp :: BinOp -> String -> String -> String
showBOp Sum x y = "("++x++"+"++y++")"
showBOp Mul x y = "("++x++"*"++y++")"


ea2ExpA' :: EA -> ExpA
-- Describe una expresión aritmética representada con el tipo ExpA, cuyo significado es el mismo que ladada.
ea2ExpA' = foldEA Cte bop2Exp

bop2Exp Sum = Suma
bop2Exp Mul = Prod

ea2Arbol' :: EA -> ABTree BinOp Int 
-- Escribe la representación como elemento del tipo ABTree BinOp Int de la expresión aritmética dada.
ea2Arbol' = foldEA Empty NodeT


e1 = BOp Sum (BOp Mul (Const 1) (BOp Mul (Const 3) (Const 2))) (Const (0))