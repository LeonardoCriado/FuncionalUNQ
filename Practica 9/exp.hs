data ExpA = Cte Int
          | Suma ExpA ExpA
          | Prod ExpA ExpA

-- f (Cte n) = ...
-- f (Suma e1 e2) = ... f e1 ... f e2
-- f (Prod e1 e2) = ... f e1 ... f e2

evalExpA :: ExpA -> Int
evalExpA (Cte n)      = n
evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2
evalExpA (Prod e1 e2) =  evalExpA e1 * evalExpA e2

simplificarExpA :: ExpA -> ExpA
simplificarExpA (Cte n) = Cte n
simplificarExpA (Suma e1 e2) = 
	simpSuma (simplificarExpA e1) (simplificarExpA e2)
simplificarExpA (Prod e1 e2) =
	simpProd (simplificarExpA e1) (simplificarExpA e2)

-- x + 0 = x
-- 0 + x = x

simpSuma :: ExpA -> ExpA -> ExpA
simpSuma e1 (Cte 0) = e1
simpSuma (Cte 0) e2 = e2
simpSuma e1 e2 = Suma e1 e2

simpProd :: ExpA -> ExpA -> ExpA
simpProd e1 (Cte 1) = e1
simpProd (Cte 1) e2 = e2
simpProd e1 (Cte 0) = Cte 0
simpProd (Cte 0) e2 = Cte 0
simpProd e1 e2 = Prod e1 e2

cantidadDeSumaCero :: ExpA -> Int
cantidadDeSumaCero (Cte n) = 0
cantidadDeSumaCero (Suma e1 e2) = 
	unoSiSumaCero e1 e2 + cantidadDeSumaCero e1 + cantidadDeSumaCero e2
cantidadDeSumaCero (Prod e1 e2) =
	cantidadDeSumaCero e1 + cantidadDeSumaCero e2

unoSiSumaCero (Cte 0) e2 = 1
unoSiSumaCero e1 (Cte 0) = 1
unoSiSumaCero e1 e2 = 0

-- -- Quiero demostrar esta equivalencia
-- ¿ cantidadSumaCero . simplificarExpA = const 0 ?

-- -- Ppio de ext.

-- Para todo e, ¿(cantidadSumaCero . simplificarExpA) e = const 0 e?

-- Por def (.) y const, esto es equivalente a

-- Para todo e, ¿cantidadSumaCero (simplificarExpA e) = 0?

-- Voy a demostrar por inducción estructural sobre ExpA, elijo un e cualquiera

-- Caso base) e = Cte n

-- ¿ cantidadSumaCero (simplificarExpA (Cte n)) = 0 ?

-- trivial

-- Caso ind 1) e = Suma e1 e2

-- HI.1) cantidadSumaCero (simplificarExpA e1) = 0
-- HI.2) cantidadSumaCero (simplificarExpA e2) = 0
-- TI) ¿cantidadSumaCero (simplificarExpA (Suma e1 e2)) = 0?

-- izq)
-- cantidadSumaCero (simplificarExpA (Suma e1 e2))
-- = -- def simplificarExpA
-- cantidadSumaCero (simpSuma (simplificarExpA e1) (simplificarExpA e2))
-- = -- lema: cantidadSumaCero (simpSuma e1 e2) = cantidadSumaCero e1 + cantidadSumaCero e2
-- cantidadDeSumaCero (simplificarExpA e1) + cantidadDeSumaCero (simplificarExpA e2)
-- = -- HI)
-- 0 + 0
-- =
-- 0

-- -- Lema cantidadSumaCero-simpSuma
-- Para todo e1 y e2, cantidadSumaCero (simpSuma e1 e2) =
-- 	cantidadSumaCero e1 + cantidadSumaCero e2

-- -- Voy a demostrar por casos sobre e1 y e2

-- Caso 1) e2 = Cte 0 y e1 cualquier expA

-- ¿ cantidadSumaCero (simpSuma e1 (Cte 0)) = 
-- 	 cantidadSumaCero e1 + cantidadSumaCero (Cte 0) ?

-- -- lado izq
-- cantidadSumaCero (simpSuma e1 (Cte 0))
-- = -- def simpSuma
-- cantidadSumaCero e1

-- -- lado der
-- cantidadSumaCero e1 + cantidadSumaCero (Cte 0)
-- = -- def cantidadSumaCero
-- cantidadSumaCero e1 + 0
-- = -- arit.
-- cantidadSumaCero e1

-- Caso 2) e1 = Cte 0 y e2 cualquier otro caso

-- ¿ cantidadSumaCero (simpSuma (Cte 0) e2) = 
-- 	 cantidadSumaCero (Cte 0) + cantidadSumaCero e2 ?

-- -- lado izq
-- cantidadSumaCero (simpSuma (Cte 0) e2)
-- = -- def simpSuma
-- cantidadSumaCero e2

-- -- lado der
-- cantidadSumaCero (Cte 0) + cantidadSumaCero e2
-- = -- def cantidadSumaCero
-- 0 + cantidadSumaCero e2
-- = -- arit.
-- cantidadSumaCero e2

-- Caso 3) e1 y e2 son cualquier otro caso

-- ¿ cantidadSumaCero (simpSuma e1 e2) = 
-- 	 cantidadSumaCero e1 + cantidadSumaCero e2 ?

-- -- lado izq
-- cantidadSumaCero (simpSuma e1 e2)
-- = -- def simpSuma
-- cantidadSumaCero (Suma e1 e2)
-- = -- def cantidadSumaCero
-- unoSiSumaCero e1 e2 + cantidadDeSumaCero e1 + cantidadDeSumaCero e2
-- = -- def unoSiSumaCero
-- 0 + cantidadDeSumaCero e1 + cantidadDeSumaCero e2
-- = -- arit.
-- cantidadDeSumaCero e1 + cantidadDeSumaCero e2

-- -- Queda demostrado el lema, por casos.



-- Caso ind 2) e = Prod e1 e2

-- HI.1) cantidadSumaCero (simplificarExpA e1) = 0
-- HI.2) cantidadSumaCero (simplificarExpA e2) = 0
-- TI) ¿cantidadSumaCero (simplificarExpA (Prod e1 e2)) = 0?

-- izq)
-- cantidadSumaCero (simplificarExpA (Prod e1 e2))
-- = -- def simplificarExpA
-- cantidadSumaCero (simpProd (simplificarExpA e1) (simplificarExpA e2))
-- = -- lema, Para todo e1 y e2, cantidadSumaCero (simpProd e1 e2) = cantidadSumaCero e1 + cantidadSumaCero e2
-- cantidadSumaCero (simplificarExpA e1) + cantidadSumaCero (simplificarExpA e2)
-- = -- HI)
-- 0 + 0
-- =
-- 0

-- Para todo e1 y e2, 
--   cantidadSumaCero (simpProd e1 e2) = 
--   	  cantidadSumaCero e1 + cantidadSumaCero e2

-- Caso 1) e2 = Cte 1 y e1 cualquier otra caso
-- Caso 2) e1 = Cte 1 y e2 cualquier otro caso
-- Caso 3) e2 = Cte 0 y e1 cualquier otro caso
-- Caso 4) e1 = Cte 0 y e2 cualquier otro caso
-- Caso 5) e1 y e2 cualquier otro caso

-- tarea

-- simplificarExpA (Prod (Prod (Cte 0) (Cte 3)) (Cte 3))
-- -> -- def simplificarExpA
-- simpProd (simplificarExpA (Prod (Cte 0) (Cte 3))) 
--          (simplificarExpA (Cte 3))
-- -> -- def simplificarExpA x2
-- simpProd (simpProd (simplificarExpA (Cte 0))
--                    (simplificarExpA (Cte 3)))
--          (Cte 3)
-- -> -- def simplificarExpA x2
-- simpProd (simpProd (Cte 0)
--                    (Cte 3))
--          (Cte 3)
-- -> -- def simpProd
-- simpProd (Cte 0)
--          (Cte 3)
-- -> -- def simpProd
-- Cte 0

-- simplificarExpA :: ExpA -> ExpA
-- ...
-- simplificarExpA (Prod (Cte 0) e2) = 
-- 	Cte 0
-- simplificarExpA (Prod e1 (Cte 0)) = 
-- 	Cte 0
-- simplificarExpA (Prod e1 e2) = 
-- 	Prod (simplificarExpA e1) (simplificarExpA e2)
-- ...

-- simplificarExpA (Prod (Prod (Cte 0) (Cte 3)) (Cte 3))
-- -> -- def simplificarExpA
-- simplificarExpA 
-- -> 
-- Prod (simplificarExpA (Prod (Cte 0) (Cte 3)))
--      (simplificarExpA (Cte 3))
-- -> -- def simplificarExpA x2
-- Prod (Cte 0) (Cte 3)

-- data ExpA = Cte Int 
--           | BOp BinOp ExpA ExpA

-- data BinOp = Suma | Prod

-- -- f (Cte n) = ...
-- -- f (BOp op e1 e2) = ... f e1 ... f e2

-- evalExpA :: ExpA -> Int
-- evalExpA (Cte n)      = n
-- evalExpA (BOp op e1 e2) = evalBOp op (evalExpA e1) (evalExpA e2)

-- evalBOp Suma = (+)
-- evalBOp Prod = (*)
