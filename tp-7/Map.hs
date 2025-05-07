module Map
    (Map, emptyM, assocM, lookupM, deleteM, keys)
        where

data Map k v = M [(k,v)]
{- INV.REP.: en donde [(k,v)]:
        * el 1er valor par nunca se repite
        -}

emptyM :: Map k v
--Propósito: devuelve un map vacío

assocM :: Eq k => k -> v -> Map k v -> Map k v
--Propósito: agrega una asociación clave-valor al map.

lookupM :: Eq k => k -> Map k v -> Maybe v
--Propósito: encuentra un valor dado una clave.

deleteM :: Eq k => k -> Map k v -> Map k v
--Propósito: borra una asociación dada una clave.

keys :: Map k v -> [k]
--Propósito: devuelve las claves del map.

emptyM             = M []
assocM k v (M kvs) = (M (asociar k v kvs))
lookupM k (M kvs)  = buscarLaClaveEn k kvs
deleteM k (M kvs)  = (M (eliminar k kvs))
keys (M kvs)       = todasLasKeysDe kvs

--Prop: asocia el par (k,v) en la lista dada, si no esta repetida.
asociar :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
asociar k v []            = (k,v):[]
asociar k v ((k',v'):kvs) = if k == k'
                                then (k',v') : kvs
                                else (k',v') : (asociar k v kvs)

buscarLaClaveEn :: Eq k => k -> [(k,v)] -> Maybe v
buscarLaClaveEn k []            = Nothing
buscarLaClaveEn k ((k',v'):kvs) = if k == k'
                                    then Just v'
                                    else buscarLaClaveEn k kvs

eliminar :: Eq k => k -> [(k,v)] -> [(k,v)]
eliminar _ []            = []
eliminar k ((k',v'):kvs) = if k == k'
                            then kvs
                            else (k',v') : eliminar k kvs

todasLasKeysDe :: [(k,v)] -> [k]
todasLasKeysDe []          = []
todasLasKeysDe ((k,_):kvs) = k : todasLasKeysDe kvs