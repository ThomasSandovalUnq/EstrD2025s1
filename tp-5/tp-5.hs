--TP5
--EJERCICIO 1.
{-head' :: [a] -> a
head' (x:xs) = x    --O(1)

sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 --O(1)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1) --O(n)

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs --O(n)

factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs --O(n)

pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs --O(n)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =
if pertenece x xs
then sinRepetidos xs
else x : sinRepetidos xs --O(n^2)

-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys --O(n)

concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs --O(n)

takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs --O(n)

dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs --O(n)

partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs) --O(n^2)

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs) --O(n)

sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) =
if n == x
then xs
else x : sacar n xs             --O(n)

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar xs =
let m = minimo xs
in m : ordenar (sacar m xs) --O(n^2)
-}

{-===================================================
    SET
===================================================-}

--imports

import SetV1
--import SetV2
import QueueV1
--import QueueV2
import StackV1

setVacio :: Set a
setVacio = emptyS

setFacil :: Set Int
setFacil = addS 1 (addS 2 (addS 3 (addS 4 (addS 5 (addS 6 emptyS)))))

setLoco :: Set Int
setLoco = addS 1 (addS 1 (addS 1 (addS 1 (addS 1 (addS 1 emptyS)))))


--2. Como usuario del tipo abstracto Set implementar las siguientes funciones:

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] _       = []
losQuePertenecen (x:xs) set = if pertenece x (setToList set)
                                then x : (losQuePertenecen xs set)
                                else losQuePertenecen xs set

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece a (x:xs) = a == x || pertenece a xs

--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos lista = setToList (construirConjunto lista)

construirConjunto :: Eq a => [a] -> Set a
construirConjunto []     = emptyS
construirConjunto (x:xs) = addS x (construirConjunto xs)

--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos del arbol.
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT            = emptyS
unirTodos (NodeT set ai ad) = unionS set (unionS (unirTodos ai) (unirTodos ad))

{-===================================================
    QUEUE
===================================================-}

queueFacil :: Queue Int
queueFacil = enqueue 10 (enqueue 1 (emptyQ))

queueFacil' :: Queue Int
queueFacil' = enqueue 8 (enqueue 0 (enqueue (-54) (emptyQ)))

{-
LA EFICIENCIA DE AMBAS QUEUE:
            QueueV1                         QueueV2
emptyQ	    O(1)	                        O(1)
isEmptyQ	O(1)	                        O(1)
enqueue	    O(n) (por la concatenación ++)	O(1) (añadir al principio de la lista :)
firstQ	    O(1) (head)	                    O(n) (last)
dequeue	    O(1) (tail)	                    O(n) (init)

3. Como usuario del tipo abstracto Queue implementar las siguientes funciones:
lengthQ :: Queue a -> Int
Cuenta la cantidad de elementos de la cola.
queueToList :: Queue a -> [a]
Dada una cola devuelve la lista con los mismos elementos,
donde el orden de la lista es el de la cola.
Nota: chequear que los elementos queden en el orden correcto.
unionQ :: Queue a -> Queue a -> Queue a
Inserta todos los elementos de la segunda cola en la primera.
-}

lengthQ :: Queue a -> Int
lengthQ queue = if isEmptyQ queue
                    then 0
                    else 1 + (lengthQ (dequeue queue))

queueToList :: Queue a -> [a]
queueToList queue = if isEmptyQ queue
                    then []
                    else (firstQ queue) : queueToList (dequeue queue)

unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if isEmptyQ q2
                then q1
                else (enqueue (firstQ (q2)) (unionQ q1 (dequeue q2)))

{-===================================================
    STACK
===================================================-}

--Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar :: [a] -> Stack a
apilar []     = emptySt
apilar (x:xs) = push x (apilar xs)

--Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar :: Stack a -> [a]
desapilar stack = if isEmptySt stack
                    then []
                    else (top stack) : (desapilar (pop stack)) 

--Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
--posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
insertarEnPos :: Int -> a -> Stack a -> Stack a
--PRECOND: LA POSICION N EXISTE EN LA STACK
insertarEnPos n e stack = if n == 0
                            then push e stack
                            else insertarEnPos (n-1) e (pop stack)

stackFacil :: Stack Int
stackFacil = push 23 (push 15(emptySt))