
doble x = x + x
cuadruple x = 4*x



{-
Ejercicio 1) 
Escribir ocho expresiones que denoten al número 4, diferentes a las ya
vistas (4, 2+2, 3+1, doble 2, doble (1+1)). Al menos seis deben usar
funciones (o sea, no ser de la forma de una cuenta de sumas o restas), de las
cuales: al menos dos deben usar una expresión lambda, tres deben usar doble y
una debe usar cuadruple.
Recordar que las definiciones dadas para doble y cuadruple son las siguientes
doble x = x + x
cuadruple x = 4*x
cuatro = 4

1- cuatro
2- doble (doble 1)
3- sqrt (16)
4- (\x -> 2 + (doble x)) 1
5- cuadruple 1
6- doble (cuadruple 0.5)
7- (\x -> x^2) 2
8- doble (sqrt 4)

-}
cuatro = 4


{-
Ejercicio 2)
Mostrar que la expresión doble (doble 2) puede reducirse de otras
formas que la vista en clase. Sugerencia: considerar el doble más externo, en
lugar del interno.

doble (doble 2)

(doble 2) + (doble 2)

(2+2) + (2+2)
-}

{-
Ejercicio 3)
Reducir cuadruple 2, y cuadruple (cuadruple 2). ¿Alguna de ellas puede hacerse de más de una forma?

cuadruple 2
4*2
8

cuadruple (cuadruple 2)
cuadruple (4*2)
cuadruple 8
4*8
32

cuadruple (cuadruple 2)
4*(cuadruple 2)
4*(4*2)
4*8
32

-}


-- Ejercicio 4)  Definir las funciones triple, succ, sumarDos.

triple x = x+x+x
succ x = x+1
sumarDos x = x+2

twice f = g
    where g x = f (f x)


{- 
Ejercicio 5) Comprobar que twice succ = sumarDos.
twice f = g
where g x = f (f x)

(twice succ) 2  (donde twice f = g where g x = f (f x))
→               (def de twice, con f ← succ)
g 2             (donde g x = succ (succ x))
→               (def de g, con x ← 2)
succ (succ 2)   (donde succ x = x + 1)
→               (def de succ, con x ← 2)
succ (2 + 1)
→               (def de suma)
succ 3          (donde succ x = x + 1)    
→               (def de succ, con x ← 3)
3 + 1           
→               (def de suma)
4

sumarDos 2      (donde sumarDos x =  x + 2 )
→               (def de sumarDos, con x ← 2)
2 + 2
→               (def de suma)
4


Tambien probando directamente en el interprete:

> twice succ 3
5
> sumarDos 3
5
> twice succ 10
12
> sumarDos 10
12

-}



{- ???
Ejercicio 6) Dar tres ejemplos de pares de expresiones que sean equivalentes entre
sí, donde al menos una de ellas sea una expresión atómica, pero no estén
vinculadas por reducción (además de cuadruple y \x -> 4*x). Dos de ellas
deben no contenter lambdas y al menos una debe contener una expresión no
atómica.

1- sumarDos y \x -> x+2
2- twice sumarDos y sumarCuatro (siendo sumarCuatro x = x+4)
3- 

-}

{- ???????????

Ejercicio 7) Realizar la reducción completa de ((twice twice) doble) 3.

((twice twice) doble) 3         (def twice, siendo f←twice doble)
→                               (def twice, siendo f←twice)
(g doble) 3                     (donde g x = twice (twice x))
→                               (def g, donde x←doble)
(twice (twice doble)) 3   
→                               (def twice, siendo f←twice doble)
g' 3                            (donde g' x = (twice doble) ((twice doble) x))
→                               (def twice, siendo x←3)
(twice doble) ((twice doble) 3) (donde twice f = g where g x = f (f x))
→                               (def twice, siendo f←doble)
(twice doble) (g'' 3)           (donde g'' x = doble (doble x)
→                               (def g'', donde x←3)
(twice doble) (doble(doble 3))  (donde doble x = x*2)
→                               (def doble, donde x←3)
(twice doble) (doble (3*2))     (donde doble x = x*2)
→                               (def doble, donde x←(3*2))
(twice doble) ((3*2)*2)         
→                               (def de *)
(twice doble) 12                (def twice, siendo f←twice doble)
→                               (def twice, siendo f←doble)
g''' 12                         (donde g''' x = doble ( doble x))
→                               (def g''', donde x←12)
doble (doble 12)                (donde doble x = x*2)
→                               (def doble, donde x←(12))
doble (12*2)                    (donde doble x = x*2)
→                               (def doble, donde x←(12*2))
(12*2)*2
→                               (def de *)
48

-}

{- ????????
Ejercicio 8) Dar expresiones lambda que sean equivalentes a las siguientes
expresiones:
a. triple       \x-> x*3
b. succ         \x-> x+1
c. sumarDos     \x-> x+2
d. twice        \f-> (\x-> f(f x))
e. twice twice  \f-> (\x-> f(f(f(f x))))                        ???

-}

{-??
Ejercicio 9) Estudiar cómo es la sintaxis y cómo se utilizan expresiones basadas en
let, if-then-else, case y where. Reescribir las siguientes funciones sin el uso
de let, where o if-then-else cuando sea posible.
a.  f x = let (y,z) = (x,x) 
            in y
    f x = x

b.  f (x,y) = let z = x + y 
                in g (z,y) 
                    where g (a,b) = a - b
    f (x,y) = (x+y)-y

c. f p = case p of (x,y) -> x
    f (x:_) = x                                                 ???

d. f = \p -> let (x,y) = p in y
    f = \(x,y) -> y

-}
