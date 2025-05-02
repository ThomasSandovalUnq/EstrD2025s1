--PRIORITY QUEUE

import PriorityQueue
import Map

--Ejercicio 1

--Ejercicio 2 

{-
Implementar la función heapSort :: Ord a => [a] -> [a], que dada una lista la ordena de
menor a mayor utilizando una Priority Queue como estructura auxiliar. ¾Cuál es su costo?
OBSERVACIÓN: el nombre heapSort se debe a una implementación particular de las Priority
Queues basada en una estructura concreta llamada Heap, que será trabajada en la siguiente
práctica.
-}

heapSort :: Ord a => [a] -> [a]                                 -- O(n^2)
heapSort [] = []
heapSort xs = construirHeapOrdenado emptyPQ xs

construirHeapOrdenado :: Ord a => PriorityQueue a -> [a] -> [a] -- O(n)
construirHeapOrdenado pq [] = extraerElementosOrdenados pq
construirHeapOrdenado pq (x:xs) = construirHeapOrdenado (insertPQ x pq) xs

extraerElementosOrdenados :: Ord a => PriorityQueue a -> [a]    -- O(n^2)
extraerElementosOrdenados pq = if (isEmptyPQ pq) 
                                    then []
                                    else (findMinPQ pq) : extraerElementosOrdenados (deleteMinPQ pq)

--Ejercicio 3 MAP

--Propósito: obtiene los valores asociados a cada clave del map.
valuesM :: Eq k => Map k v -> [Maybe v] -- O(n^2)
valuesM m = recorrer m (domM m)

recorrer :: Eq k => Map k v -> [k] -> [Maybe v] -- O(n)
recorrer _ []     = []
recorrer m (k:ks) = lookupM k m : recorrer m ks 

--Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas :: Eq k => [k] -> Map k v -> Bool -- O(n)
todasAsociadas [] _     = True
todasAsociadas (k:ks) m = pertenece k (domM m) && todasAsociadas ks m

pertenece :: Eq a => a -> [a] -> Bool -- O(1)
pertenece _ []     = False
pertenece a (x:xs) = a == x || pertenece a xs

--Propósito: convierte una lista de pares clave valor en un map.
listToMap :: Eq k => [(k, v)] -> Map k v -- O(n)
listToMap []          = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

--Propósito: convierte un map en una lista de pares clave valor.
mapToList :: Eq k => Map k v -> [(k, v)] 
mapToList m = completarLista m (domM m)

completarLista :: Eq k => Map k v -> [k] -> [(k, v)]
completarLista _ []      = []
completarLista m (k:ks) = case lookupM k m of 
                            Just v -> (k, v) : (completarLista m ks)
                            Nothing -> completarLista m ks

--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
agruparEq :: Eq k => [(k, v)] -> Map k [v] -- O(n^2)
agruparEq []           = emptyM
agruparEq ((k, v):kvs) = 
    let resto = agruparEq kvs
    in case lookupM k resto of
        Just vs -> assocM k (v : vs) resto
        Nothing -> assocM k [v] resto 

--Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a cada número asociado con dichas claves.
incrementar :: Eq k => [k] -> Map k Int -> Map k Int -- O(n)
incrementar [] _     = emptyM
incrementar (k:ks) m = 
    case lookupM k m of
        Just v  -> assocM k (v + 1) (incrementar ks m)
        Nothing -> incrementar ks m

--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
--una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v -- O(n)
mergeMaps m1 m2 = mergeAuxiliar (domM m1) m1 m2

mergeAuxiliar :: Eq k => [k] -> Map k v -> Map k v -> Map k v -- O(n)
mergeAuxiliar [] _ m2      = m2
mergeAuxiliar (k:ks) m1 m2 = 
    let v = lookupM k m1 
    in case v of
        Just val -> mergeAuxiliar ks m1 (assocM k val m2)
        Nothing  -> mergeAuxiliar ks m1 m2