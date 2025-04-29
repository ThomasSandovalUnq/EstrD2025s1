module PriorityQueue
    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ,)
    where

data PriorityQueue a = PQ [a]

emptyPQ :: PriorityQueue a
--Propósito: devuelve una priority queue vacía.

isEmptyPQ :: PriorityQueue a -> Bool
--Propósito: indica si la priority queue está vacía.

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
--Propósito: inserta un elemento en la priority queue.

findMinPQ :: Ord a => PriorityQueue a -> a
--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.

emptyPQ             = PQ []                         --O(1) 
isEmptyPQ (PQ xs)   = null xs                       --O(1) 
insertPQ a (PQ xs)  = PQ (a : xs)                   --O(1) 
findMinPQ (PQ xs)   = minimum xs                    --O(n) 
deleteMinPQ (PQ xs) = PQ (eliminar (minimum xs) xs) --O(n)

eliminar :: Eq a => a -> [a] -> [a]
--PRECOND: LA LISTA NO PUEDE SER VACIA
eliminar a (x:xs) = if a == x
                        then xs
                        else x : (eliminar a xs)