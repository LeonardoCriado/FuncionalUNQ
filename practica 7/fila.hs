data Color = Rojo | Negro | Azul | Verde 
             deriving Eq
-- Tarea: fila de Gobstones
data Fila = Final | Celda (Color -> Int) Fila 
            --deriving Show

-- Asistencia:
-- 1) Dar las reglas que definen al conjunto
-- inductivo Fila (basado el tipo algebraico Fila definido anteriormente)

-- Caso Base 1      : Final esta en el conjunto Fila
-- Caso inductivo 1 : Sea c un elemento del conjunto (Color -> Int), y f' un elemento del conjunto Fila, 
--                    Celda c f' pertenece al conjunto Fila


-- 2) dar forma esquemática de una función
-- definida por recursión estructural sobre Fila

-- esquema de recursión estructural sobre Fila
-- f Final        = ...
-- f (Celda c f') = ... f f'


-- 3)
nroBolitas :: Color -> Fila -> Int
nroBolitas c  Final       = 0
nroBolitas c (Celda fc f) = fc c + nroBolitas c f

hayBolitas :: Color -> Fila -> Bool
hayBolitas c  Final      = False
hayBolitas c (Celda fc f) = (intToBool . fc) c || hayBolitas c f

poner :: Color -> Int -> Fila -> Fila
poner c i Final        = Final
poner c i (Celda fc f) = if i == 0 
                          then Celda (sumarUnoSiEqColor c fc) f 
                          else Celda fc                       (poner c (i-1) f)

sumarUnoSiEqColor :: Color -> (Color -> Int) -> Color -> Int
sumarUnoSiEqColor = componerSiEqColor (1+)

componerSiEqColor :: (Int -> Int) -> Color -> (Color -> Int) -> Color -> Int 
componerSiEqColor f c fc c' =  if c == c'
                                 then (f.fc) c'
                                 else fc     c'

intToBool 1 = True
intToBool 0 = False

contenidoCeldaPrueba:: Color -> Int
contenidoCeldaPrueba c = case c of 
                              Rojo  -> 0
                              Negro -> 1
                              Azul  -> 2
                              Verde -> 3
                              _     -> 0



--nroBolitas Verde (Poner Verde 0 (Celda contenidoCeldaPrueba (Celda contenidoCeldaPrueba Final)))
