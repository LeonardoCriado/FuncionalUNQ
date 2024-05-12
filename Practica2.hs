{-
Para cada una de las siguientes expresiones decidir si poseen tipo. Si es
así indicar cuál es.
-}

a. 1 && 2 == 2                                      --No
b. 1 + if 3 < 5 then 3 else 5                       --Int
c. let par = (True, 4)                              
in (if first par then first par else second par)    --Bool
d. (doble doble) 5                                  --Int
e. doble (doble 5)                                  --Int
f. twice first                                      --No
g. (twice doble) doble                              --Int -> Int
h. (twice twice) first                              --No
i. apply apply                                      --((a -> b) -> a -> b) -> (a -> b) -> a -> b
