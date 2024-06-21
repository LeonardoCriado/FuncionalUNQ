type Record a b = [(a,b)]  
type Table a b = [ Record a b ]


select :: (Record a b -> Bool) -> Table a b -> Table a b
select = filter


project :: (a -> Bool) -> Table a b -> Table a b
project p = foldr (\x xs -> let i = filter (p.fst) x in  case i of
                                                              [] -> xs
                                                              _  ->  i : xs)
                  []

project' :: (a -> Bool) -> Table a b -> Table a b
project' p = map (filter (p.fst))

getName :: Record a b -> a
getName []     = error "El registro está vacio"
getName (x:xs) = fst x


conjunct :: (a -> Bool) -> (a -> Bool) -> a -> Bool
conjunct p1 p2 x = p1 x && p2 x

crossWith :: (a -> b -> c) -> [a] -> [b] -> [c]
crossWith f l1 = foldr (\x xs -> map (flip f x) l1 ++ xs ) []




product :: Eq a => Table a b -> Table a b -> Table a b
product t = foldr (\x xs -> concatATodas x t ++ xs ) []

concatATodas :: [a] -> [[a]] -> [[a]]
concatATodas = map.(++)

-- similar :: Record a b -> Record a b

e1 = [
  [("nombre", "Leo" ), ("apellido", "Criado"), ("Edad", "34"),("nacionalidad","arg")],
  [("nombre", "Pepe"), ("apellido", "Pompin"), ("Edad", "37"),("nacionalidad","uy" )]]   

e2 = [
  [("nombre", "Mar"   ), ("apellido", "Brabo"   ), ("Edad", "35" ), ("sexo", "M")],
  [("nombre", "Pedro" ), ("apellido", "Pedro"   ), ("Edad", "89" ), ("sexo", "X")],
  [("nombre", "Manuel"), ("apellido", "Belgrano"), ("Edad", "180"), ("sexo", "M")]]


