twice f = (\x -> f(f x))
mult = (\x-> \y-> x*y)
compose = (\f-> (\g-> (\x-> f(g x))))
suma = \x-> \y-> x+y
flip = \f-> \x-> \y-> f y x
resta = \x-> \\y -> x - y
neg = mult (-1)
id x = x