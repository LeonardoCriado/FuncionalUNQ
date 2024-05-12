type Nombre = String
data Planilla = Fin | Registro Nombre Planilla 

data Equipo = Becario Nombre
            | Investigador Nombre Equipo Equipo Equipo
            deriving Show

-- Show (show)
-- Eq (==)
-- Ord (<) (>) (>=) (<=)

-- Desafío, intentar mostrar el árbol Equipo de forma
-- legible
-- instance Show Equipo where
--     -- show :: Equipo -> String
--     show (Becario n) = "Becario { nombre: " ++ n ++ " }" 
--     show (Investigador n e1 e2 e3) = "Investigador { "

-- Defino con las siguientes reglas el conjunto inductivo Equipo

-- regla base)

-- Si n está en el conjunto Nombre,
-- entonces Becario n está en el conjunto Equipo 

-- regla inductiva)

-- Si n está en el conjuunto Nombre
-- y e1 está en el conjunto Equipo
-- y e2 está en el conjunto Equipo
-- y e3 está en el conjunto Equipo

-- entonces Investigador n e1 e2 e3 está en el conjunto Equipo

-- esquema de recursión estructural
-- sobre Equipo
-- f (Becario n) = ...
-- f (Investigador n e1 e2 e3) =
--     ... f e1
--     ... f e2
--     ... f e3

-- esquema sobre Planilla
-- f Fin = ...
-- f (Registro n p) =
--     ... f p

largo :: Planilla -> Int
largo Fin = 0
largo (Registro n p) = 1 + largo p

estaNombre :: Nombre -> Planilla -> Bool
estaNombre e Fin = False
estaNombre e (Registro n p) =
    e == n || estaNombre e p

-- no conviene esta otra forma
-- estaNombre e (Registro n p) =
--     if e == n
--        then True
--        else estaNombre e p

existeQuienCumpla :: (Nombre -> Bool) -> Planilla -> Bool
existeQuienCumpla pred Fin = False
existeQuienCumpla pred (Registro n p) =
    pred n || existeQuienCumpla pred p

todosCumplen :: (Nombre -> Bool) -> Planilla -> Bool
todosCumplen pred Fin = True
todosCumplen pred (Registro n p) =
    pred n && todosCumplen pred p

-- ningunNombreEsStringVacio = todosCumplen (not . null)

-- not :: Bool -> Bool
-- null :: [a] -> Bool

juntarPlanillas :: Planilla -> Planilla -> Planilla
juntarPlanillas Fin pl2 = pl2
juntarPlanillas (Registro n p) pl2 =
    Registro n (juntarPlanillas p pl2)

-- indica si dos planillas son iguales
sonIguales :: Planilla -> Planilla -> Bool
sonIguales Fin Fin = True
sonIguales Fin pl2 = False
sonIguales pl1 Fin = False
sonIguales (Registro n1 p1) (Registro n2 p2) =
   n1 == n2 && sonIguales p1 p2

nivelesJerarquicos :: Equipo -> Int
nivelesJerarquicos (Becario n) = 0
nivelesJerarquicos (Investigador n e1 e2 e3) =
    1 +
    nivelesJerarquicos e1
    `max`
    nivelesJerarquicos e2
    `max`
    nivelesJerarquicos e3

-- max x y =
--     if x > y
--        then x
--        else y

-- 1 `max` 2 `max` 3 `max` 4

-- 4

cantidadDeIntegrantes :: Equipo -> Int
cantidadDeIntegrantes (Becario n) = 1
cantidadDeIntegrantes (Investigador n e1 e2 e3) =
    1
    + cantidadDeIntegrantes e1
    + cantidadDeIntegrantes e2
    + cantidadDeIntegrantes e3

planillaDeIntegrantes :: Equipo -> Planilla
planillaDeIntegrantes (Becario n) = Registro n Fin
planillaDeIntegrantes (Investigador n e1 e2 e3) =
    Registro n
        (planillaDeIntegrantes e1
            `juntarPlanillas` 
            planillaDeIntegrantes e2
            `juntarPlanillas` 
            planillaDeIntegrantes e3)

-- Tarea:
planillaDebecariosNivelN :: Int -> Equipo -> Planilla
planillaDebecariosNivelN l (Becario n)               = if l == 0 then Registro n Fin else Fin
planillaDebecariosNivelN l (Investigador n e1 e2 e3) = if l == 0 
                                                          then Fin
                                                          else
                                                          (planillaDebecariosNivelN (l-1) e1
                                                            `juntarPlanillas` 
                                                           planillaDebecariosNivelN (l-1) e2
                                                           `juntarPlanillas` 
                                                           planillaDebecariosNivelN (l-1) e3)


todosSeLlaman :: Nombre -> Planilla -> Bool
todosSeLlaman s Fin = True
todosSeLlaman s (Registro n p)
   s == n && todosSeLlaman s p