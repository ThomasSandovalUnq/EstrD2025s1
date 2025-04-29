module Map
    (Map, emptyM, assocM, lookupM, deleteM, keys)
        where

data Map k v = M [(k,v)]

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
assocM k v (M kvs) = 
lookupM k (M kvs)  =
deleteM k (M kvs)  =
keys (M kvs)       = 