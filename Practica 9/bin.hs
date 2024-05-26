data DigBin = O | I

type NBin = [DigBin]

-- f [] = ...
-- f (x:xs) = ... f xs

-- version 1:
evalNB :: NBin -> Int
evalNB []     = 0
evalNB (b:bs) = evalDB b + 2 * evalNB bs

evalDB O = 0
evalDB I = 1

-- -- version 2:
-- evalNB :: NBin -> Int
-- evalNB bs = evalNB' 0 bs

-- evalNB' :: NBin -> Int
-- evalNB' p []     = 0
-- evalNB' p (b:bs) = evalDB' b * (2^p) + evalNB' (p+1) bs

-- -- version 3:
-- evalNB :: NBin -> Int
-- evalNB bs = evalNB' 1 bs

-- evalNB' :: NBin -> Int
-- evalNB' p []     = 0
-- evalNB' p (b:bs) = evalDB' b * p + evalNB' (p*2) bs

normalizarNB :: NBin -> NBin
normalizarNB [] = []
normalizarNB (b:bs) = normD b (normalizarNB bs)

normD O [] = []
normD b bs = b : bs

-- normalizarNB :: NBin -> NBin
-- normalizarNB xs = reverse (dropWhile0 (reverse xs))

-- dropWhile0 _ [] = []
-- dropWhile0 O xs = dropWhile0 xs
-- dropWhile0 I xs = I : xs

-- evalNB . normalizarNB = evalNB

-- -- ppio de ext.

-- Para todo xs, (evalNB . normalizarNB) xs = evalNB xs

-- Por def (.), es equivalente a

-- Para todo xs, evalNB (normalizarNB xs) = evalNB xs

-- -- Elijo un xs cualquiera,
-- -- y voy a demostrar por inducción estructural de listas sobre xs

-- Caso base) xs = []

-- ¿ evalNB (normalizarNB []) = evalNB [] ?

-- izq)
-- evalNB (normalizarNB [])
-- = -- def normalizarNB
-- evalNB []
-- der)

-- -- ambos lados son iguales, el caso base es verdadero

-- Caso ind) xs = (z:zs)

-- HI) evalNB (normalizarNB zs) = evalNB zs
-- TI) ¿ evalNB (normalizarNB (z:zs)) = evalNB (z:zs) ?

-- der)
-- evalNB (z:zs)
-- = -- def evalNB
-- evalDB z + 2 * evalNB zs
-- = -- HI
-- evalDB z + 2 * evalNB (normalizarNB zs)
-- = -- lema, d = z, ds = normalizarNB zs
-- evalNB (normD z (normalizarNB zs))

-- izq)
-- evalNB (normalizarNB (z:zs))
-- = -- def normalizarNB
-- evalNB (normD z (normalizarNB zs))

-- ambos lados son iguales, queda demostrado el caso inductivo

-- ------------------------------------------------------

-- Lema)
-- Para todo d, ds. 
-- evalDB d + 2 * evalNB ds = evalNB (normD d ds)

-- Elijo un d y ds cualesquiera

-- ¿ evalDB d + 2 * evalNB ds = evalNB (normD d ds) ?

-- Caso 1) d = O y ds = []

-- izq)
-- evalDB O + 2 * evalNB []
-- = -- def evalDB y evalNB
-- 0 + 2 * 0
-- = -- arit.
-- 0

-- der)
-- evalNB (normD 0 [])
-- = -- def normD
-- evalNB []
-- = -- def evalNB
-- 0

-- Caso 2) cualquier otro caso

-- der)
-- evalNB (normD d ds)
-- = -- def normD
-- evalNB (d:ds)
-- = -- def evalNB
-- evalDB d + 2 * evalNB ds

-- Quedó demostrado para todos los casos de d y ds, por lo que la propiedad, es verdadera.

