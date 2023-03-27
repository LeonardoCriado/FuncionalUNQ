compose::(b -> c) -> ((a -> b) -> (a->c))
compose f = h
    where h g = k where k x = f (g x)



subst::(a -> b) -> ((a -> b) -> (a -> c))
subst f = h 
 where h g = k where k x = (f x) (g x)    



