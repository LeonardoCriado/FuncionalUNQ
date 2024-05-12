data Color = Rojo | Negro | Azul | Verde

-- Tarea: fila de Gobstones
data Fila = Final
          | Celda (Color -> Int) Fila

-- Asistencia:
-- 1) Dar las reglas que definen al conjunto
-- inductivo Fila (basado el tipo algebraico Fila definido anteriormente)
-- 2) dar forma esquemática de una función
-- definida por recursión estructural sobre Fila
-- 3)
nroBolitas :: Color -> Fila -> Int
nroBolitas = undefined

hayBolitas :: Color -> Fila -> Bool
hayBolitas = undefined

poner :: Color -> Int -> Fila -> Fila
poner = undefined
