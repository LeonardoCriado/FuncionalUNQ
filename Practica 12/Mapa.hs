data Dir = Left | Right | Straight
data Mapa a = Cofre [a]
             | Nada (Mapa a)
             | Bifurcacion [a] (Mapa a) (Mapa a)


foldM fc fn fb (Cofre xs)             = fc xs
foldM fc fn fb (Nada m)               = 
foldM fc fn fb (Bifurcacion xs m1 m2) =    
