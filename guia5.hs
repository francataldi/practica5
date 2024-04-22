--EJERCICIO 1
--Ej 1.1
longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud (xs)
--Ej 1.2
ultimo :: [t] -> t
ultimo [x] = x
ultimo (x:xs) = ultimo xs 
--Ej 1.3
principio :: [t] -> [t]
principio (x:[]) = []
principio (x:xs) = x : principio xs    
--Ej 1.4
reverso :: [t] -> [t]
reverso [] = []
reverso [x] = [x]
reverso (x:xs) =  ultimo xs : reverso (principio (x:xs)) 
--EJERCICIO 2
--Ej 2.1
pertenece :: (Eq t) => t -> [t] -> Bool 
pertenece e [] = False
pertenece e (x:xs) = e == x || pertenece e xs 
--Ej 2.2
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True 
todosIguales (x:[]) = True
todosIguales (x:xs) = x == head xs && todosIguales xs 
--Ej 2.3
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True 
todosDistintos (x:[]) = True
todosDistintos (x:xs) = not (pertenece x xs) && todosDistintos xs
--Ej 2.4