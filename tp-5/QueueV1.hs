module QueueV1
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
    where

data Queue a = Q [a]
    {-
    INV. REP: 
        *Se agrega elementos al final de la lista.
        *Para quitar un elemento debe de ser el 1ero.
    -}

emptyQ :: Queue a
isEmptyQ :: Queue a -> Bool
enqueue :: a -> Queue a -> Queue a
firstQ :: Queue a -> a
dequeue :: Queue a -> Queue a

emptyQ           = (Q [])
isEmptyQ (Q xs)  = null xs
enqueue a (Q xs) = agregarA a xs
firstQ (Q xs)    = head xs
dequeue (Q xs)   = sinElPrimerElemento xs

agregarA :: a -> [a] -> Queue a
agregarA a [] = (Q (a:[]))
agregarA a xs = (Q (xs ++ [a]))

sinElPrimerElemento :: [a] -> Queue a
--PRECOND: La lista no puede ser vacia
sinElPrimerElemento (x:xs) = Q (xs)