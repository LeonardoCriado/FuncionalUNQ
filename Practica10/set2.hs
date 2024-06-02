type Set a = (a -> Bool)

belongs :: Eq a => a -> Set a -> Bool
belongs = undefined 

size :: Set a -> Int
size = undefined 

add :: Eq a => a -> Set a -> Set a
add = undefined 

remove :: Eq a => a -> Set a -> Set a
remove = undefined 

union :: Eq a => Set a -> Set a -> Set a
union = undefined 

intersection :: Eq a => Set a -> Set a -> Set a
intersection = undefined 

