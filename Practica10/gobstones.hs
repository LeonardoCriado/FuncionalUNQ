data Color = Rojo | Azul deriving Eq

data Fila = Fin
          | Celda [Color] Fila

-- f Fin = ...
-- f (Celda cs fila) = ... f fila

-- lineal
-- Celda [Rojo, Rojo, Azul, Azul]
--       (Celda [] 
--           (Celda [Rojo] Fin))

data Cmd = Poner Int Color
         | Sacar Int Color
         | Repeat Int Cmd
         | Seq Cmd Cmd
         | Skip

type Program = Cmd

-- Tarea
-- evalCmd . normCmd = evalCmd
-- evalCmd . sinRepeat = evalCmd

evalCmd :: Cmd -> Mem -> Fila -> Fila
evalCmd (Poner i c) f = poner i c f
evalCmd (Sacar i c) f =  sacar i c f
evalCmd (Repeat n cmd) f = 
    repeatCmd n (evalCmd cmd f)
evalCmd (Seq cmd1 cmd2) f =
    evalCmd cmd2 (evalCmd cmd1 f)
evalCmd Skip f = f

-- Seq (Poner i c) (Sacar i c) = Skip
-- Seq (Poner i c) (Poner i c) = Repeat 2 (Poner i c)
-- Seq (Sacar i c) (Sacar i c) = Repeat 2 (Sacar i c)
-- Repeat 0 c = Skip
-- Repeat n (Repeat m c) = Repeat (n*m) c
normCmd :: Cmd -> Cmd
normCmd (Poner i c) = Poner i c
normCmd (Sacar i c) = Sacar i c
normCmd (Repeat n cmd) =
    normRepeat n (normCmd cmd)
normCmd (Seq cmd1 cmd2) =
    normSeq (normCmd cmd1) (normCmd cmd2)
normCmd Skip = Skip

normRepeat :: Int -> Cmd -> Cmd
normRepeat 0 c = Skip
normRepeat n (Repeat m c) = Repeat (n*m) c
normRepeat n cmd = Repeat n cmd

normSeq :: Cmd -> Cmd -> Cmd
normSeq (Poner i1 c1) (Sacar i2 c2) =
    if i1 == i2 && c1 == c2
       then Skip
       else Seq (Poner i1 c1) (Sacar i2 c2)
normSeq (Poner i1 c1) (Poner i2 c2) =
    if i1 == i2 && c1 == c2
       then Repeat 2 (Poner i1 c1)
       else Seq (Poner i1 c1) (Poner i2 c2)
normSeq (Sacar i1 c1) (Sacar i2 c2) = 
    if i1 == i2 && c1 == c2
       then Repeat 2 (Sacar i1 c1)
       else Seq (Sacar i1 c1) (Sacar i2 c2)    
normSeq cmd1 cmd2 = Seq cmd1 cmd2

-- Tarea
sinRepeat :: Cmd -> Cmd
sinRepeat = undefined

-- Tarea
-- chequea si los índices son válidos
programaIncorrecto :: Cmd -> Fila -> Bool
programaIncorrecto = undefined

-- Tarea: agregar variables al lenguaje

poner :: Int -> Color -> Fila -> Fila
poner n c Fin = Fin
poner 0 c (Celda cs fila) = 
    Celda (c:cs) fila
poner n c (Celda cs fila) =
    Celda cs (poner (n-1) c fila)

-- tail (x:xs) = xs

sacar :: Int -> Color -> Fila -> Fila
sacar n c Fin = Fin
sacar 0 c (Celda cs fila) = 
    Celda (tail cs) fila
sacar n c (Celda cs fila) =
    Celda cs (sacar (n-1) c fila)

-- version 1
-- ponerN :: Int -> Int -> Color -> Fila -> Fila
-- ponerN 0 i c f = id
-- ponerN n i c f = poner i c (ponerN (n-1) i c f)

ponerN :: Int -> Int -> Color -> Fila -> Fila
ponerN n i c Fin = Fin
ponerN n 0 c (Celda cs fila) = 
    Celda (replicate n c ++ cs) fila
ponerN n i c (Celda cs fila) =
    Celda cs (ponerN n i c fila)

hayBolitas :: Int -> Fila -> Bool
hayBolitas n Fin = False
hayBolitas 0 (Celda cs fila) = 
    not (null cs)
hayBolitas n (Celda cs fila) =
    hayBolitas (n-1) fila

apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x:xs) =
    if e == x
       then 1 + apariciones e xs
       else apariciones e xs

    -- (if e == x then 1 else 0) + apariciones e xs

nroBolitas :: Int -> Color -> Fila -> Int
nroBolitas n c Fin = 0
nroBolitas 0 c (Celda cs fila) = 
    apariciones c cs
nroBolitas n c (Celda cs fila) =
    nroBolitas (n-1) c fila

-- Tarea 1:
-- Para todo i, c, fila. nroBolitas i c fila > 0 = hayBolitas i c fila

-- Tarea 2:
-- Para n, i, c, fila. many n (poner i c) fila = ponerN n i c fila
-- many 0 f x = x
-- many n f x = f (many (n-1) f x)


