data ExpA =  Cte Int | Sum ExpA ExpA | Prod ExpA ExpA deriving (Show , Eq)

e1 = Sum (Prod (Cte 1) (Prod (Cte 1) (Cte 0))) (Cte 1)

cantidadDeCeros :: ExpA -> Int
cantidadDeCeros = foldExpA (unoSi (==0)) (+) (+)

unoSi f e = if f e then 1 else 0

notieneNEgativosExplicitosExpA :: ExpA -> Bool
notieneNEgativosExplicitosExpA = foldExpA (>=0) (&&) (&&)

simplificarExpA' :: ExpA -> ExpA
simplificarExpA' = foldExpA Cte
                            (\x -> \y -> simplSuma x y)
                            (\x -> \y -> simplProd x y)

simplSuma:: ExpA -> ExpA -> ExpA
simplSuma (Cte 0) y       = y
simplSuma x       (Cte 0) = x
simplSuma x       y       = Sum x y

simplProd:: ExpA -> ExpA -> ExpA
simplProd (Cte 0) y       = Cte 0
simplProd (Cte 1) y       = y
simplProd x       (Cte 0) = Cte 0
simplProd x       (Cte 1) = x
simplProd x       y       = Prod x y




showExpA :: ExpA -> String
showExpA = foldExpA show  
                    (\x y -> "(" ++ x ++ "+" ++ y ++ ")")
                    (\x y -> "(" ++ x ++ "*" ++ y ++ ")")

recExpA:: (Int -> b) -> (b -> ExpA -> b -> ExpA -> b) -> (b -> ExpA -> b -> ExpA -> b) -> ExpA -> b
recExpA c s p (Cte i)    = c i
recExpA c s p (Sum x y)  = s (recExpA c s p x) x (recExpA c s p y) y
recExpA c s p (Prod x y) = p (recExpA c s p x) x (recExpA c s p y) y

cantDeSumaCeros :: ExpA -> Int
cantDeSumaCeros = recExpA (\i -> 0)
                          (\xr x yr y -> (unoSiCte 0 x `max` unoSiCte 0 y) + xr + yr)
                          (\xr x yr y -> xr + yr)

unoSiCte :: Int -> ExpA -> Int
unoSiCte c (Cte x) = if c == x then 1 else 0
unoSiCte c _       = 0


cantDeProdUnos :: ExpA -> Int
cantDeProdUnos = recExpA (\i -> 0)
                         (\xr x yr y -> xr + yr)
                         (\xr x yr y -> (unoSiCte 1 x `max` unoSiCte 1 y) + xr + yr)

evalExpA :: ExpA -> Int
evalExpA (Cte n)      = n
evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2
evalExpA (Prod e1 e2) =  evalExpA e1 * evalExpA e2

evalExpA' :: ExpA -> Int
evalExpA' = foldExpA id (+) (*)

foldExpA:: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA c s p (Cte i)    = c i
foldExpA c s p (Sum x y)  = s (foldExpA c s p x) (foldExpA c s p y)
foldExpA c s p (Prod x y) = p (foldExpA c s p x) (foldExpA c s p y)

-- DEMO:
-- ¿ evalExpA' = evalExpA ?
-- 
-- Por ppio de extensionalidad
-- 
-- Para todo x.
-- ¿ evalExpA' x = evalExpA x ?
-- 
-- Sea x un elemento de ExpA cualquiera, por inducción en la estructura de x:
-- 
-- CB) x = Cte n , siendo n un Int cualquiera
-- ¿ evalExpA' (Cte n) = evalExpA (Cte n) ?
-- 
-- CI 1) x = Suma e1 e2
-- HI 1) ¡ evalExpA' e1 = evalExpA e1 !
-- HI 2) ¡ evalExpA' e2 = evalExpA e2 !
-- TI) ¿ evalExpA' (Suma e1 e2) = evalExpA (Suma e1 e2) ?
-- 
-- CI 2) x = Prod e1 e2
-- HI 1) ¡ evalExpA' e1 = evalExpA e1 !
-- HI 2) ¡ evalExpA' e2 = evalExpA e2 !
-- TI) ¿ evalExpA' (Prod e1 e2) = evalExpA (Prod e1 e2) ?
-- 
-- CB:
-- IZQ)
--   evalExpA' (Cte n)
-- =                               (evalExpA')
--   (foldExpA id (+) (*)) (Cte n)
-- =                               (foldExpA)
--   id n
-- =                               (id)
--   n <-----------------------------------------
--                                              |
-- DER)                                         |
--   evalExpA (Cte n)                           =
-- =                               (evalExpA)   |
--   n <----------------------------------------|
-- 
-- 
-- CI 1:
-- IZQ)
--   evalExpA' (Suma e1 e2)
-- =                                LEMA 1: evalExpA' (Suma x y) = evalExpA' x + evalExpA' y
--   evalExpA' e1 + evalExpA' e2
-- =                                (HI 1, HI 2)
--   evalExpA e1 + evalExpA e2 <----------------|
--                                              |
-- DER)                                         |
--   evalExpA (Suma e1 e2)                      =
-- =                                (evalExpA)  |
--   evalExpA e1 + evalExpA e2 <----------------|
-- 
-- 
-- CI 2:
-- IZQ)
--   evalExpA' (Prod e1 e2)
-- =                                LEMA 2: evalExpA' (Prod x y) = evalExpA' x + evalExpA' y
--   evalExpA' e1 * evalExpA' e2
-- =                                (HI 1, HI 2)
--   evalExpA e1 * evalExpA e2 <----------------|
--                                              |
-- DER)                                         |
--   evalExpA (Prod e1 e2)                      =
-- =                                (evalExpA)  |
--   evalExpA e1 * evalExpA e2 <----------------|
-- 
-- LEMA 1:
-- ¿ evalExpA' (Suma x y) = evalExpA' x + evalExpA' y ?
-- 
-- IZQ)
--   evalExpA' (Suma x y)
-- =                                                        (evalExpA')
--   foldExpA id (+) (*) (Suma x y)
-- =                                                        (foldExpA)
--   (+) (foldExpA id (+) (*) x) (foldExpA id (+) (*) y)
-- =                                                        (evalExpA')
--   (+) (evalExpA' x) (evalExpA' y)
-- =                                                        (+)
--   evalExpA' x + evalExpA' y
-- 
-- LEMA 2:
-- ¿ evalExpA' (Prod x y) = evalExpA' x * evalExpA' y ?
-- 
-- IZQ)
--   evalExpA' (Prod x y)
-- =                                                        (evalExpA')
--   foldExpA id (+) (*) (Prod x y)
-- =                                                        (foldExpA)
--   (*) (foldExpA id (+) (*) x) (foldExpA id (+) (*) y)
-- =                                                        (evalExpA')
--   (*) (evalExpA' x) (evalExpA' y)
-- =                                                        (+)
--   evalExpA' x * evalExpA' y
-- 
-- Queda entonces demostrado que evalExpA' = evalExpA