
module QueueO
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
    where

data Queue a = Q [a]

emptyQ :: Queue a
isEmptyQ :: Queue a -> Bool
enqueue :: a -> Queue a -> Queue a
firstQ :: Queue a -> a
dequeue :: Queue a -> Queue a

emptyQ           = (Q [])
isEmptyQ (Q xs)  = isEmpty xs
enqueue a (Q xs) = agregarA a xs
firstQ (Q xs)    = head xs
dequeue (Q xs)   = sinElPrimerElemento xs

agregarA :: Eq a => a -> [a] -> Queue a
agregarA a []     = (Q (a:[]))
agregarA a xs = (Q (xs ++ [a]))

sinElPrimerElemento :: [a] -> [a]
--PRECOND: La lista no puede ser vacia
sinElPrimerElemento (x:xs) = xs