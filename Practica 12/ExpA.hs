data ExpA =  Cte Int | Sum ExpA ExpA | Prod ExpA ExpA deriving (Show , Eq)

e1 = Sum (Prod (Cte 1) (Prod (Cte 1) (Cte 0))) (Cte 1)

foldExpA:: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA c s p (Cte i)    = c i
foldExpA c s p (Sum x y)  = s (foldExpA c s p x) (foldExpA c s p y)
foldExpA c s p (Prod x y) = p (foldExpA c s p x) (foldExpA c s p y)


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


evalExpA' :: ExpA -> Int
evalExpA' = foldExpA id (+) (*)

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
