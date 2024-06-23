type Record a b = [(a,b)]  
type Table a b = [ Record a b ]


select :: (Record a b -> Bool) -> Table a b -> Table a b
select = filter

project :: (a -> Bool) -> Table a b -> Table a b
project p = map (filter (p.fst))

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
compactProyect p1 q                 = Projection p1               q

compactSelect :: (Record a b -> Bool) -> Query a b -> Query a b
compactSelect p1 (Selection p2 q) = Selection (conjunct p1 p2) q
compactSelect p1 q                = Selection p1               q

e1 = Projection (/= "age")
        (Selection
          (\r -> any (\(c,v)-> c == "name" && v == "Edward Snowden") r)
          (Table [ [("name", "Edward Snowden"), ("age", "29")],
                   [("name", "Jason Bourne"), ("age", "40")] ]))

DEMO:
¿ execute . compact = execute ?

por ppio de extencionalidad

para todo x.
¿ (execute . compact) x = execute x ?

por def de (.)

para todo x.
¿ execute (compact x) = execute x ?

Sea x un elemento de "Query a b" por principio de induccion en la estructura de x.

CB) x =  Table r , siendo r un elemento cualquiera de "Table a b"
¿ execute (compact (Table r)) = execute (Table r) ?

CI1) x = Product q1 q2
HI1) ¡ execute (compact q1) = execute q1 !
HI2) ¡ execute (compact q2) = execute q2 !
TI) ¿ execute (compact (Product q1 q2)) = execute (Product q1 q2) ?

CI2) x = Projection p q1
HI1) ¡ execute (compact q1) = execute q1 !
TI) ¿ execute (compact (Projection p q1)) = execute (Projection p q1) ?

CI3) x = Selection p q1
HI1) ¡ execute (compact q1) = execute q1 !
TI) ¿ execute (compact (Selection p q1)) = execute (Selection p q1) ?


CB:
IZQ)
  execute (compact (Table r))
=                                                         (compact)
  execute (foldQ Table
                (\q1 q2 -> Product q1 q2)
                (\p q1 -> compactProyect p q1)
                (\p q1 -> compactSelect p q1) (Table r))
=                                                         (foldQ)
  execute (Table r)

CI1:
IZQ)
  execute (compact (Product q1 q2))
=                                                         (compact)
  execute (foldQ Table
                (\q1 q2 -> Product q1 q2)
                (\p q1 -> compactProyect p q1)
                (\p q1 -> compactSelect p q1) 
                (Product q1 q2)
          )
=                                                         (foldQ)
  execute ((\q1 q2 -> Product q1 q2) 
            (foldQ Table
                   (\q1 q2 -> Product q1 q2)
                   (\p q1 -> compactProyect p q1)
                   (\p q1 -> compactSelect p q1)
                   q1) 
            (foldQ Table
                   (\q1 q2 -> Product q1 q2)
                   (\p q1 -> compactProyect p q1)
                   (\p q1 -> compactSelect p q1)
                   q2)
          )
=                                                        (compact)
  execute ((\q1 q2 -> Product q1 q2) 
            (compact q1) 
            (compact q2))
=                                                        (lambda)
 execute (Product (compact q1) (compact q2))
=                                                        (execute)
 (foldQ id
       (\q1 q2 -> productT q1 q2)
       (\p q1  -> project p q1)
       (\p q1  -> select p q1)) (Product (compact q1) 
                                         (compact q2))
=                                                         (foldQ)
 (\q1 q2 -> productT q1 q2) (foldQ id
                                   (\q1 q2 -> productT q1 q2)
                                   (\p q1  -> project p q1)
                                   (\p q1  -> select p q1) (compact q1))
                            (foldQ id
                                   (\q1 q2 -> productT q1 q2)
                                   (\p q1  -> project p q1)
                                   (\p q1  -> select p q1) (compact q2))
=                                                        (execute)
 (\q1 q2 -> productT q1 q2) (execute (compact q1)) 
                            (execute (compact q2))
=                                                        (H1 y H2)
 (\q1 q2 -> productT q1 q2) (execute q1) (execute q2) <------------------|
                                                                         |
DER)                                                                     |
  execute (Product q1 q2)                                                |
=                                                              (execute) |
 (foldQ id                                                               |
       (\q1 q2 -> productT q1 q2)                                        |
       (\p q1  -> project p q1)                                          |
       (\p q1  -> select p q1)) (Product q1 q2)                          |
=                                                              (foldQ)   |
 (\q1 q2 -> productT q1 q2) (foldQ id                                    =
                                   (\q1 q2 -> productT q1 q2)            |
                                   (\p q1  -> project p q1)              |
                                   (\p q1  -> select p q1) q1)           |
                            (foldQ id                                    |
                                   (\q1 q2 -> productT q1 q2)            |
                                   (\p q1  -> project p q1)              |
                                   (\p q1  -> select p q1) q2)           |
=                                                              (execute) |
 (\q1 q2 -> productT q1 q2) (execute q1) (execute q2) <------------------|


CI2) x = Projection p q1
HI) ¡ execute (compact q1) = execute q1 !
TI) ¿ execute (compact (Projection p q1)) = execute (Projection p q1) ?

CI2:
IZQ)
  execute (compact (Projection p q1))
=                                                        (compact)
  execute (foldQ Table
                (\q1 q2 -> Product q1 q2)
                (\p q1 -> compactProyect p q1)
                (\p q1 -> compactSelect p q1) (Projection p q1))
=                                                        (foldQ)
  execute ((\p q1 -> compactProyect p q1) p
            (foldQ Table
                   (\q1 q2 -> Product q1 q2)
                   (\p q1 -> compactProyect p q1)
                   (\p q1 -> compactSelect p q1)
                   q1) 
          )
=                                                         (compact)
  execute ((\p q1 -> compactProyect p q1) p (compact q1)) 
=                                                         (lambda)
  execute (compactProyect p (compact q1))
=                                                         LEMA: execute (compactProyect p q) = execute (Projection p q)
  execute (Projection p (compact q1))
=                                                         (execute)              
  proyect p (execute (compact q1))
=                                                         HI
  proyect p (execute q1) <-------------------------------------------|
                                                                     |
DER)                                                                 |
  execute (Projection p q1)                                          =
=                                                         (execute)  |
  project p (execute q1) <-------------------------------------------|

LEMA:

Para todo p. Para todo q.
¿ execute (compactProyect p q) =  execute (Projection p q) ?

CASO 1:
q = Projection p2 q

IZQ)
  execute (compactProyect p (Projection p2 q))
=                                                (compactProyect)
  execute (Projection (conjunct p p2) q)
=                                                (execute)
  project (conjunct p p2) (execute q)

DER)
  execute (Projection p (Projection p2 q))
=                                                (execute)
  project p (execute (Projection p2 q))
=                                                (execute)
  project p (project p2 (execute q))
=                                                 LEMA2:  project p (project p2 t) = project (conjunct p p2) t
  project (conjunct p p2) (execute q)

CASO 2:
q /= Projection p2 q

IZQ)
  execute (compactProyect p q)
=
  execute (Projection p q)

LEMA 2:
Para todo p. Para todo p2. Para todo q.
¿ project p (project p2 t) = project (conjunct p p2) t ?

DER)
  project (conjunct p p2) t
=
  map (filter (conjunct p p2).fst ) t

IZQ)
  project p (project p2 t)
= 
  project p (map (filter (p2.fst)) t)
=
  map (filter (p.fst) . filter (p2.fst)) t
=                                                LEMA3: (filter p . filter p2) x = filter (conjunct p p2) x              
  map (filter (conjunct (p.fst) (p2.fst)) ) t
=
  map (filter (conjunct p p2).fst ) t 


LEMA3
Para todo p. Para todo p2. Para todo xs.
¿ (filter p (filter p2 xs)) = filter (conjunct p p2) xs ?

IZQ
