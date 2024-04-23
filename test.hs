import System.Win32 (xBUTTON1)
-- EJERCICIO 1a --
longitud :: [a] -> Integer --dada una lista, devuelve su cantidad de elementos
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- EJERCICIO 1b --
ultimo :: [a] -> a
ultimo [a] = a
ultimo (x:xs) =  ultimo xs  -- tengo que devolver el ultimo elemento de la  lista

-- EJERCICIO 1c --
principio :: [a] -> [a]             -- tiene que devolverme la misma lista, pero sin el ultimo elemento
principio [a] = []                  -- esto lo uso para q cuando quede el ultimo elemento de la cola, lo descarte y no lo agregue
principio (x:xs) = x : principio xs -- hago que una todos los primeros elementos, a las respectivas colas

-- quiero combinar x recursivamente con las correspondientes colas de la lista (xs)
-- ejemplo: 
-- principio [1,2,3,4] = [1,2,3]

-- EJERCICIO 1d --
inversa :: [a] -> [a]                              -- quiero devolver los elementos de una lista, pero en orden inverso
inversa [] = []                                    -- le marco cuando debe cortar la recursión
inversa xs = ultimo xs : inversa (principio xs)    -- quiero lograr combinar ultimo xs con inversa de lo que queda de la lista sin el ultimo elemento, y lograr dar vuelta la lista

-- ejemplo:
-- inversa [1,2,3,4] = [4,3,2,1]
-- = 4 : inversa([1,2,3]) => inversa ([1,2,3]) = ultimo [1,2,3] (3) : inversa ([1,2]) = ultimo [1,2,3] (3) : ultimo [1,2] (2) : inversa ([1])......... 
-- = 4 : 3 : inversa ([1,2])
-- = 4 : 3 : 2 : inversa([1])
-- = 4 : 3 : 2 : 1 : inversa([])
-- = [4,3,2,1]

-- EJERCICIO 2a --
pertenece :: (Eq a) => a -> [a] -> Bool
pertenece x [] = False
pertenece x (y:ys)  | x == y = True                -- si x = y, entonces es True
                    | otherwise = pertenece x ys   -- si x/= y, entonces fijate si x pertenece a la lista que queda

-- ejemplo:
-- pertenece 1 [1,2,3] = True, pues 1 pertenece a [1,2,3]

-- EJERCICIO 2b --
todosIguales :: (Eq a) => [a] -> Bool -- con ayuda
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:ys) = x == y && todosIguales (y : ys)

-- ejemplo:
-- todosIguales [1,1,1] = True
-- 1 == 1? si!, entonces hago todos iguales de y : ys
-- que seria fijarme si el 1 = [1]? si!

-- EJERCICIO 2c --
todosDistintos :: (Eq a) => [a] -> Bool
todosDistintos [] = True
todosDistintos [x] = True
todosDistintos (x:y:ys) |x == y = False
                        |otherwise = todosDistintos (y : ys) 
   
-- ejemplo:
-- todosDistintos [1,2,3,4] = True
-- todosDistintos [1,2,3,2] = False 

-- funcionamiento:
-- 1 = 2? no, entones me fijo el resto de la lista
-- 2 = 3? no, entonces me fijo el resto de la lista
-- 2 = 2? si, entonces devuelvo false

-- EJERCICIO 2d --
hayRepetidos :: (Eq a) => [a] -> Bool
hayRepetidos [] = False
hayRepetidos [x] = False
hayRepetidos (x:y:ys) | x == y = True
                      | otherwise = hayRepetidos (y : ys)

-- ejemplo:
-- hayRepetidos [1,2,2] = True
-- pues: x == Y? no, entonces me fijo si Y = YS
-- pues: Y == YS? si, entonces devuelvo True

-- EJERCICIO 2e --
quitar :: (Eq a) => a -> [a] -> [a]  -- con ayuda
quitar x [] = []
quitar x (y:ys) | x == y = ys
                | otherwise = y : quitar x ys

-- ejemplo:
-- quitar 3 [1,2,3] = [1,2]

--funcionamiento:
-- quitar 3 [1,2,3] seria algo asi:
-- 3 = 1? no, entonces 1 : la lista que no va a tener ese x que le quité
-- 3 = 2? no, entonces 1 : 2 : la lista que no va a tener ese x que le quité
-- 3 = 3? si, entonces devuelvo la cola de la lista, en este caso seria []
-- y me terminaría quedando 1 : 2 : [], siendo ([]), la lista que no tiene ese x que le quité
-- me quedaría: [1,2]

-- EJERCICIO 2f --
quitarTodos :: (Eq a) => a -> [a] -> [a]
quitarTodos x [] = []
quitarTodos x (y:ys) | x == y = quitarTodos x ys                          -- si x = y, saca x de la cola
                     | otherwise = y : quitarTodos x ys    -- sino, uni x a la lista a la que le quite todos los x repetidos

-- ejemplo:
-- quitarTodos 1 [1,2,3,1] = [2,3]

-- funcionamiento:
-- quitarTodos 1 [1,2,3,1] = [2,3]
-- 1 = 1?, si, entonces vamos a quitar 1 de la lista
-- 1 = 2?, no, entonces metemos el 2 a la lista que no tiene el 1 , pues pedí quitarlo
-- 1 = 3?, no, entonces metemos el 3 a la lista que no tiene el 1, pues pedí quitarlo
-- 1 = 1?, si, entonces vamos a quitar 1 de la lista

-- EJERCICO 2g --
eliminarRepetidos :: (Eq a) => [a] -> [a]   -- con ayuda
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs)

-- ejemplo:
-- eliminarRepetidos [1,2,3,3,2,1] = [1,2,3]

-- funcionamiento:
-- 1 : una nueva lista que va a quitar todos los numeros 1 que quedan dentro de la lista => 1 : eliminarRepetidos (quitarTodos (1 [2,3,3,2,1])) = 1 : [2,3,3,2]
-- 2 : una nueva lista que va a quitar todos los numeros 2 que quedan dentro de la lista => 2 : eliminarRepetidos (quitarTodos (2 [3,3,2,1]))   = 2 : [3,3,1]
-- 3 : una nueva lista que va a quitar todos los numeros 3 que quedan dentro de la lista => 3 : eliminarRepetidos (quitarTodos (3 [2,3,3,2,1])) = 3: [2,3,2,1]
-- y asi llamaria recursivamente hasta ir eliminando todo

-- EJERCICIO 2h --
mismosElementos :: (Eq a) => [a] -> [a] -> Bool -- pedir ayuda
mismosElementos s r = todosPertenecen s r && todosPertenecen r s

todosPertenecen :: (Eq a) => [a] -> [a] -> Bool  -- funcion aux
todosPerteneces [] _ = True
todosPertenecen (x:xs) ys = pertenece x ys && todosPerteneces xs ys  

-- ejemplo:
-- mismosElementos [1,2,3] [3,2,1] = True
-- mismosElementos [1,2,3] [3,2,2] = False

-- EJERCICIO 2i --
capicua :: (Eq a) => [a] -> Bool
capicua [x] = True
capicua (x:xs) = x == ultimo xs    -- es tan simple como que si el primer elemento, no es el mismo que el ultimo, entonces no son capicua :p

-- ejemplo:
-- capicua [´a’,’c’, ’b’, ’b’, ’c’, ´a’] = True
-- capicua [´a’, ’c’, ’b’, ’d’, ´a’]     = False
-- capica [1,2,3,2,1] = True

-- funcionamiento:
-- capicua [1,2,3,2,1], es true si es capicua, osea que se lee igual de forma invertida que de forma original

-- EJERCICIO 3a --
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- EJERCICIO 3b --
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- EJERCICIO 3c --
maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:y:xs) | x < y = maximo (y:xs)
                | x > y = maximo (x:xs)
                | otherwise = maximo (x:y:xs)

-- ejemplo:
-- maximo [1,3,4,7] = 7
-- maximo [] = 0
-- maximo [3,2,8] = 8

-- funcionamiento:
-- si x < y, directamente fijate cual es el maximo entre y & xs
-- si x > y, directamente fijate cual es el maximo entre x & xs
-- si x = y, fijate el maximo entre la lista que va con x:y:xs 

-- EJERCICIO 3d --
sumarN :: Integer -> [Integer] -> [Integer]
sumarN x [] = []
sumarN x (y:ys) = (x + y) : sumarN x ys

-- ejemplo:
-- sumarN 1 [1,2,3] = [2,3,4]

-- funcionamiento:
-- sumarN 1 [1,2,3]
-- = [1 + 1, 1 + 2, 1 + 3]
-- = 1 (x) + 1 (y) : [2,3]
-- = 1 (x) + 1 (y) : 1 (x) + 2 (y) : [3]
-- = 1 (x) + 1 (y) : 1 (x) + 2 (y) :  1 (x) + 3 (y) : []
-- = [2,3,4]

-- EJERCICIO 3e --
sumarElPrimero :: [Integer] -> [Integer]  -- con ayuda
sumarElPrimero [x] = [(2*x)]
sumarElPrimero (x:xs) = sumarN x (x:xs)  -- sumale el numero X a cada termino de la lista (x:xs), asi de simple

-- ejemplo:
-- sumarElPrimero [1,2,3] = [2,3,4]

-- funcionamiento:
-- sumarElPrimero [1,2,3]
-- = [ 1 + 1, 2 + 1, 3 + 1]
-- = [2,3,4]

-- EJERCICIO 3f --
sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo [x] = [(2*x)]
sumarElUltimo (x:xs) = sumarN (ultimo xs) (x:xs)  -- sumale el ultimo numero de la lista a cada termino de la lista (x:xs)

-- ejemplo:
-- sumarElUltimo [1,2,3] = [4,5,6]

-- EJERCICIO 3g --
pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) | mod x 2 == 0 = x : pares xs
             | otherwise = quitar x (pares xs)

-- ejemplo:
-- pares [1,2,3,4] = [2,4]
-- pares [1,2,3,6,8,4,12] = [2,6,8,4,12]

-- EJERCICIO 3h --
multiplosDeN :: Integer -> [Integer] -> [Integer]  -- pedir ayuda porque no me devuelve x como multiplo de si mismo
multiplosDeN x [] = []
multiplosDeN x (y:ys) | mod y x == 0 = y : multiplosDeN x ys
                      | otherwise    = quitar x (multiplosDeN x ys)

-- ejemplo:
-- multiplosDeN 4 [1,2,3,4,5,6,7,8] = [4,8]

-- EJERCICIO 3i --
ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar [x] = [x]
ordenar (x:y:xs) = masChico (x:y:xs) : ordenar (quitar (masChico (x:y:xs))(x:y:xs)) -- minimo de la lista, unido a quitar el minimo a la lista sin el primer minimo
-- agarro primero el elemento mas chico
-- despues junto al elemento mas chico, con una nueva lista que va a ser el resultado de ordenar a la lista a la cual le saque su elemento minimo

-- me quedaria por ejemplo:
-- ordenar [1,3,2]
-- = saco el minimo (1) : ordenar la lista que tiene su minimo elemento afuera (2 : [3])
-- = 1 : (minimo 2 : [3]), y por definicion el minimo de [3] es 3
-- = entonce me quedaria:
-- 1 (min [1,3,2]) : (min[3,2]) : (min[min[3,2]])
-- = min [1,3,2] : min [3,2] : min [3]
-- = 1 : 2 : 3 = [1,2,3]

masChico :: [Integer] -> Integer
masChico [x] = x
masChico (x:y:xs) | x < y = masChico (x:xs) -- como se q X es menor a Y, fijate si X es menor a los demas tmb
                  | y < x = masChico (y:xs)
                  | otherwise = masChico (x:y:xs)

-- ejemplo:
-- ordenar [1,3,5,67,8] = [1,3,5,8,67] => ordena de forma creciente

-- funcionamiento:
-- ordenar [1,3,2] = [1,2,3]