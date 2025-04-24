module QueueV3
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
    where

data Queue a = Q [a] [a]
                 fs   bs
    {-
    INV. REP: 
        *Se quitan elementos a traves de la lista fs.
        *Se agregan elementos a traves de la lista bs.
        *Si la lista fs se encuentra vacia, entonces la cola se encuentra vacia.
    -}

emptyQ   :: Queue a
isEmptyQ :: Queue a -> Bool
enqueue  :: a -> Queue a -> Queue a
firstQ   :: Queue a -> a
dequeue  :: Queue a -> Queue a

emptyQ              = (Q [] [])
isEmptyQ (Q fs bs)  = null fs
enqueue a (Q fs bs) = 
firstQ (Q fs bs)    = 
dequeue (Q fs bs)   = 