module MultiSet 
    --(MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList)
    (MultiSet, emptyMS, addMS, ocurrencesMS, multiSetToList)
    where

import Map

data MultiSet a = MS (Map a Int)


-- INV.REP.: El Map interno asocia cada elemento del MultiSet con su número de ocurrencias (siempre mayor que 0).

emptyMS :: MultiSet a
addMS :: Ord a => a -> MultiSet a -> MultiSet a
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
--unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
--intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
multiSetToList :: Eq a => MultiSet a -> [(a, Int)]

emptyMS                 = MS emptyM

addMS a (MS map)        = MS (assocM a (ocurrencesMS a (MS map) + 1) map)

ocurrencesMS a (MS map) = case lookupM a map of
                            Nothing -> 0
                            Just count -> count

multiSetToList (MS map) =  mapToList map

mapToList :: Eq k => Map k v -> [(k, v)] 
mapToList m = completarLista m (domM m)

completarLista :: Eq k => Map k v -> [k] -> [(k, v)]
completarLista _ []      = []
completarLista m (k:ks) = case lookupM k m of 
                            Just v -> (k, v) : (completarLista m ks)
                            Nothing -> completarLista m ks 