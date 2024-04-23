-- EJERCICIO 2.1 --
pertenece :: (Eq a) => a -> [a] -> Bool
pertenece x [] = False
pertenece x (y:ys) = x == y || pertenece x ys

-- EJERCICIO 2.4 --
hayRepetidos :: (Eq a) => [a] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | pertenece x xs = True
                    | otherwise = hayRepetidos xs

-- EJERCICIO 2.5 --
quitar :: (Eq a) => a -> [a] -> [a]
quitar x [] = []
quitar x (y:ys) | x == y = ys
                | otherwise = y : quitar x ys

-- EJERCICIO 3.3 -- 
maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:y:ys) | x > y = maximo (x:ys)       -- como X es mas grande que Y, compara X con el resto de la lista sin Y
                | y > x = maximo (y:ys)       -- como Y es mas grande que X, compara Y con el testode la lista sin X
                | otherwise = maximo (x:ys)   -- sino, segui haciendo maximo de X unido con YS

-- EJERCICIO 3.9 --
ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar [x] = [x]
ordenar (x:y:ys) = minimo (x:y:ys) : (ordenar (quitar (minimo (x:y:ys)) (x:y:ys))) -- minimo de la lista, unido a quitar el minimo a la lista sin el primer minimo
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

-- FUNCION AUXILIAR --
minimo :: [Integer] -> Integer
minimo [x] = x 
minimo (x:y:ys) | x < y = minimo (x:ys)
                | y < x = minimo (y:ys)
                | otherwise = minimo (x:ys)

-- ejemplo funcion:
-- ordenar [1,4,7,3,2]                =  [1,2,3,4,7]
-- 1 : [4,7,3,2]                      =  1 : [2,3,4,7]
-- 1 : 4 : [7,3,2]                    =  1 : 2 : [3,4,7]
-- 1 : 4 : 7 : [3,2]                  =  1 : 2 : 3 : [4,7]
-- 1 : 4 : 7 : 3 : [2]                =  1 : 2 : 3 : 4 : [7]
-- 1 : 4 : 7 : 3 : 2 : []             =  1 : 2 : 3 : 4 : 7 : []

-- EJERCICIO 3.1 --
sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- EJERCICIO 3.2 -- 
productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- EJERCICIO 3.4 --
sumarN :: Integer -> [Integer] -> [Integer]
sumarN x [] = []
sumarN x (y:ys) = (x + y) : sumarN x ys             -- quiero poder sumarle x a cada termino de sumarN (y:ys), siendo esa la lista que le doy a la funcion :)
                                                    -- tengo que ver como hacer para sumarle x a cada termino individualmente de la lista que puse de input
                                                    -- osea q tengo q combinar algo del tipo: x + ......... sumarN ........
-- Ejemplo Función:
-- sumarN 1 [1,2,3] = [1+1, 1+2, 1+3]
-- seria igual a : [ 1 + head[1,2,3], 1 + head[2,3], 1 + head[3], 1 + head[] ]

-- EJERCICIO 3.5 --
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero [x] = [x + x]
sumarElPrimero (x:xs) = x + x : sumarElPrimero xs

-- Ejemplo FUnciòn:
-- sumarElPrimero [1,2,3] => [2,3,4], osea que le sume el primer elementop de la lista a cada elemento de la lista



