module QueueV2
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
    where

data Queue a = Q [a]
    {-
    INV. REP: 
        *Se agrega elementos al por delante de la lista.
        *Para quitar un elemento se quita por detras.
    -}

emptyQ :: Queue a
isEmptyQ :: Queue a -> Bool
enqueue :: a -> Queue a -> Queue a
firstQ :: Queue a -> a
dequeue :: Queue a -> Queue a

emptyQ           = (Q [])
isEmptyQ (Q xs)  = null xs
enqueue a (Q xs) = (Q (a:xs))
firstQ (Q xs)    = last xs
dequeue (Q xs)   = quitarElemento xs

quitarElemento :: [a] -> Queue a
--PRECOND: La lista no puede ser vacia
quitarElemento xs = Q (init xs)