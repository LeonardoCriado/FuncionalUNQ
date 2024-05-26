--import Prelude hiding ( )


data NExp = Var Variable
            | NCte Int
            | NBOp NBinOp NExp NExp
            deriving Show 
data NBinOp = Add | Sub | Mul | Div | Mod | Pow deriving Show 
type Variable = String


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



