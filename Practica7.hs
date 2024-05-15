


--Ejercicio 1)
-- Dar las reglas que definen el conjunto inductivo correspondiente a la
-- parte totalmente definida del tipo algebraico Pizza definido por:
data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa deriving Show


--Ejercicio 3)
cantDeCapas:: Pizza -> Int
cantDeCapas Prepizza    = 0
cantDeCapas (Capa _ p)  = 1 + (cantDeCapas p)


cantidadDeAceitunas:: Pizza -> Int
cantidadDeAceitunas Prepizza                = 0
cantidadDeAceitunas (Capa (Aceitunas i) p)  = i + (cantidadDeAceitunas p)
cantidadDeAceitunas (Capa _ p)              = 0 + (cantidadDeAceitunas p)

duplicarAceitunas:: Pizza -> Pizza
duplicarAceitunas Prepizza                = Prepizza
duplicarAceitunas (Capa (Aceitunas i) p)  = Capa (Aceitunas (i*2)) (duplicarAceitunas p)
duplicarAceitunas (Capa i p)              = Capa i (duplicarAceitunas p)


sinLactosa:: Pizza -> Pizza
sinLactosa Prepizza       = Prepizza
sinLactosa (Capa Queso p) = sinLactosa p
sinLactosa (Capa i     p) = Capa i (sinLactosa p)


aptaIntolerantesLactosa:: Pizza -> Bool
aptaIntolerantesLactosa p = cantDeCapas p == cantDeCapas (sinLactosa p)


aptaIntolerantesLactosa':: Pizza -> Bool
aptaIntolerantesLactosa' Prepizza        = True
aptaIntolerantesLactosa' (Capa Queso p)  = False
aptaIntolerantesLactosa' (Capa i p)      = aptaIntolerantesLactosa' p


conDescripcionMejorada:: Pizza -> Pizza
conDescripcionMejorada Prepizza               = Prepizza
conDescripcionMejorada (Capa (Aceitunas i) p) = juntarAceitunas i (conDescripcionMejorada p)
conDescripcionMejorada (Capa i p)             = Capa i (conDescripcionMejorada p)


juntarAceitunas:: Int -> Pizza -> Pizza
juntarAceitunas n (Capa (Aceitunas i) p) = Capa (Aceitunas (n+i)) p
juntarAceitunas n p                      = Capa (Aceitunas n) p



-- Dar las reglas que definen el conjunto inductivo correspondiente a la
--parte totalmente definida de los tipos algebraicos Planilla y Equipo dados por las
--siguientes definiciones:
type Nombre = String

data Planilla = Fin | Registro Nombre Planilla
-- Def. inductiva del conjunto Planilla
-- Regla base 1      : Fin está en Planilla
-- Regla inductiva 1 : Si e está en el conjunto Registro Nombre e está en Planilla

data Equipo = Becario Nombre | Investigador Nombre Equipo Equipo Equipo
-- Def. inductiva del conjunto Equipo
-- Regla base 1      : Becario Nombre está en Equipo
-- Regla inductiva 1 : Si e, e' y e'' está en el conjunto, Investigador Nombre e e' e'' está en Planilla

--Ejercicio 2
---Planilla
--  f Fin          = a
--  f Registro n p = g n (f p)

---Equipo
--  f Becario      n          = a
--  f Investigador n e e' e'' = g n (f e) (f e') (f e'')


--Ejercicio 3) 
--Dar el tipo de cada una de las siguientes funciones, y definirlas utilizando recursión estructural.


largoDePlanilla:: Planilla -> Int
--describe la cantidad de nombres en una planilla dada.
largoDePlanilla Fin            = 0
largoDePlanilla (Registro _ p) = 1 + (largoDePlanilla p)

esta:: Nombre -> Planilla -> Bool
--Toma un nombre y una planilla e indica si en la planilla dada está el nombre dado.
esta n Fin             = False
esta n (Registro n' p) = (n==n') || (esta n p)

juntarPlanillas:: Planilla -> Planilla -> Planilla
--toma dos planillas y genera una única planilla con los registros de ambas planillas.
juntarPlanillas p              Fin              = p
juntarPlanillas Fin            p                = p
juntarPlanillas (Registro n p) (Registro n' p') = Registro n (Registro n' (juntarPlanillas p p'))

nivelesJerarquicos:: Equipo -> Int
--describe la cantidad de niveles jerárquicos de un equipo dado.
nivelesJerarquicos (Becario n)                = 0
nivelesJerarquicos (Investigador n e e' e'')  = 1 + 
                                                nivelesJerarquicos e
                                                `max` 
                                                nivelesJerarquicos e' 
                                                `max` 
                                                nivelesJerarquicos e''

cantidadDeIntegrantes:: Equipo -> Int
--describe la cantidad de integrantes de un equipo dado.
cantidadDeIntegrantes (Becario n)                = 1
cantidadDeIntegrantes (Investigador n e e' e'')  = 1 + (cantidadDeIntegrantes e  )
                                                     + (cantidadDeIntegrantes e' )
                                                     + (cantidadDeIntegrantes e'')

planillaDeIntegrantes:: Equipo -> Planilla
--describe la planilla de integrantes de un equipo dado.
planillaDeIntegrantes (Becario n)               = Registro n Fin
planillaDeIntegrantes (Investigador n e e' e'') = Registro n (
                                                     juntarPlanillas 
                                                        (planillaDeIntegrantes e) 
                                                        (juntarPlanillas 
                                                            (planillaDeIntegrantes e' )
                                                            (planillaDeIntegrantes e'') 
                                                        )
                                                  )



becariosNivelN:: Int -> Equipo -> Planilla
becariosNivelN = undefined




