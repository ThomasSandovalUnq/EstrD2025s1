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
    SETV1
===================================================-}

--import SetV1
import SetV2

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

{-3. Implementar la variante del tipo abstracto Set que posee una lista y admite repetidos. En
otras palabras, al agregar no va a chequear que si el elemento ya se encuentra en la lista, pero
sí debe comportarse como Set ante el usuario (quitando los elementos repetidos al pedirlos,
por ejemplo). Contrastar la eficiencia obtenida en esta implementación con la anterior.-}

{-
La eficiencia que se obtiene en la nueva implementacion de SetV2 es mas ineficiente que la de SetV1, ya que al agregar todos los elementos
sin necesidad de sacar a los repetidos, ya que en el addS tardarian lo mismo aproximadamente, para hacer la union debo de hacer una eleccion 
para cuando pongo, o no, si hay un elemento que se repite muchas veces en el Set y lo quiero sacar, este va a sacarlo todas las veces
que aparezca, por lo tanto la eficiencia de SetV2 es menor a la de SetV1.
-}

{-
3. Queue (cola)
Una Queue es un tipo abstracto de datos de naturaleza FIFO (rst in, rst out). Esto signica
que los elementos salen en el orden con el que entraron, es decir, el que se agrega primero es el
primero en salir (como la cola de un banco). Su interfaz es la siguiente:
emptyQ :: Queue a
Crea una cola vacía.
isEmptyQ :: Queue a -> Bool
Dada una cola indica si la cola está vacía.
enqueue :: a -> Queue a -> Queue a
Dados un elemento y una cola, agrega ese elemento a la cola.
firstQ :: Queue a -> a
Dada una cola devuelve el primer elemento de la cola.
dequeue :: Queue a -> Queue a
Dada una cola la devuelve sin su primer elemento.
1. Implemente el tipo abstracto Queue utilizando listas. Los elementos deben encolarse por el
nal de la lista y desencolarse por delante
-}

