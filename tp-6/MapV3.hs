module MapV3
    (Map, emptyM, assocM, lookupM, deleteM, domM)
        where

data Map k v = M [k] [v]
{- INV.REP.: en donde :
    * La lista de claves no contiene elementos repetidos.
    * Ambas listas tienen la misma longitud.
    * El elemento en la posición i de la lista de claves está asociado con el elemento en la posición i de la lista de valores.
        -}

emptyM :: Map k v
--Propósito: devuelve un map vacío

assocM :: Eq k => k -> v -> Map k v -> Map k v
--Propósito: agrega una asociación clave-valor al map.

lookupM :: Eq k => k -> Map k v -> Maybe v
--Propósito: encuentra un valor dado una clave.

deleteM :: Eq k => k -> Map k v -> Map k v
--Propósito: borra una asociación dada una clave.

domM :: Map k v -> [k]
--Propósito: devuelve las claves del map.

emptyM               = M [] []                      --O(1)

assocM k v (M ks vs) = if elem k ks                 --O(n)
                        then M ks vs
                        else M (k:ks) (v:vs)

lookupM k (M ks vs)  = buscarLaClaveEn k ks vs      --O(n)

deleteM k (M ks vs)  = if elem k ks                 --O(n)
                        then M ks vs
                        else M  (eliminar k ks) (eliminarValor (posicionDe k ks) vs)

domM (M ks vs)       = ks                           --O(1)

buscarLaClaveEn :: Eq k => k -> [k] -> [v] -> Maybe v
buscarLaClaveEn _ [] _       = Nothing
buscarLaClaveEn k (k':ks) (v:vs) = if k == k'
                                    then Just v
                                    else buscarLaClaveEn k ks vs

--PRECOND: DEBE EXISTIR EL ELEMENTO DADO, EN LA LISTA DADA.
eliminar :: Eq k => k -> [k] -> [k]
eliminar _ []      = []
eliminar k (k':ks) = if k == k'
                        then ks
                        else k': (eliminar k ks)

eliminarValor :: Int -> [v] -> [v]
eliminarValor _ []     = []
eliminarValor n (v:vs) = if n == 0
                            then vs
                            else v : eliminarValor (n-1) vs

--PRECOND: EL ELEMENTO DADO DEBE DE EXISTIR EN LA LISTA DADA.
posicionDe :: Eq k => k -> [k] -> Int
posicionDe _ []      = error "TIENE QUE EXISTIR EL ELEMENTO k EN LA LISTA DADA"
posicionDe k (k':ks) = if k == k'
                        then 0
                        else 1 + (posicionDe k ks)