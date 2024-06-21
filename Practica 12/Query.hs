type Record a b = [(a,b)]  
type Table a b = [ Record a b ]


select :: (Record a b -> Bool) -> Table a b -> Table a b
select = filter

project :: (a -> Bool) -> Table a b -> Table a b
project p = foldr (\x xs -> let i = filter (p.fst) x in  case i of
                                                              [] -> xs
                                                              _  ->  i : xs) []

productT :: Eq a => Table a b -> Table a b -> Table a b
productT t = foldr (\x xs -> concatATodas x t ++ xs ) []

concatATodas :: [a] -> [[a]] -> [[a]]
concatATodas = map.(++)

conjunct :: (a -> Bool) -> (a -> Bool) -> a -> Bool
conjunct p1 p2 x = p1 x && p2 x


data Query a b = Table [ Record a b ]
                 | Product (Query a b) (Query a b)
                 | Projection (a -> Bool) (Query a b)
                 | Selection (Record a b -> Bool) (Query a b)


foldQ :: ([ Record a b ] -> c) -> (c -> c -> c) 
          -> ((a -> Bool) -> c -> c) -> ((Record a b -> Bool)-> c -> c) 
          -> Query a b -> c
foldQ ft fp fpr fs (Table      t    ) = ft t
foldQ ft fp fpr fs (Product    q1 q2) = fp (foldQ ft fp fpr fs q1) (foldQ ft fp fpr fs q2)
foldQ ft fp fpr fs (Projection p  q ) = fpr p (foldQ ft fp fpr fs q)
foldQ ft fp fpr fs (Selection  p  q ) = fs  p (foldQ ft fp fpr fs q) 


tables :: Query a b -> [Table a b] --que describe la lista de todas las tablas involucradas en la query dada.
tables = foldQ (\t -> [t])
               (\q1 q2 -> q1 ++ q2)
               (\p q1 -> q1)
               (\p q1 -> q1)

execute :: Eq a => Query a b -> Table a b -- que describe el resultado de ejecutar la query dada.
execute = foldQ id
                (\q1 q2 -> productT q1 q2)
                (\p q1  -> project p q1)
                (\p q1  -> select p q1)

compact :: Query a b -> Query a b 
-- describe la query resultante de compactar las selecciones y proyecciones consecutivas en la query dada
compact = foldQ Table
                (\q1 q2 -> Product q1 q2)
                (\p q1 -> compactProyect p q1)
                (\p q1 -> compactSelect p q1)

compactProyect :: (a -> Bool) -> Query a b -> Query a b
compactProyect p1 (Projection p2 q) = Projection (conjunct p1 p2) q
compactProyect p1 q                 = Projection p1 q

compactSelect :: (Record a b -> Bool) -> Query a b -> Query a b
compactSelect p1 (Selection p2 q) = Selection (conjunct p1 p2) q
compactSelect p1 q                = Selection p1 q

e1 = Projection (/= "age")
        (Selection
          (\r -> any (\(c,v)-> c == "name" && v == "Edward Snowden") r)
          (Table [ [("name", "Edward Snowden"), ("age", "29")],
                   [("name", "Jason Bourne"), ("age", "40")] ]))
