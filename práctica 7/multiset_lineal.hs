data Multiset a = EmptyMS
                | Apariciones a Int (Multiset a)

-- denota un multiset vacío
-- (no contiene apariciones de ningún elemento)
emptyMS :: Multiset a

-- indica la cantidad de apariciones
count :: Eq a => a -> Multiset a -> Int

-- incrementa una aparición
add :: Eq a => a -> Multiset a -> Multiset a

-- decrementa una aparición
remove :: Eq a => a -> Multiset a -> Multiset a

-- junta dos multiset
-- donde las apariciones para cada elemento
-- son sumadas
union :: Multiset a -> Multiset a -> Multiset a

-- junta dos multiset
-- donde las apariciones para cada elemento
-- corresponde a la menor cantidad de apariciones
-- entre ellos
intersection :: Multiset a -> Multiset a -> Multiset a