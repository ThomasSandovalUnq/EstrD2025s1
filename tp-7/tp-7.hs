data Tree a = EmptyT | NodeT a (Tree a) (Tree a)


--1. Propósito: dado un BST dice si el elemento pertenece o no al árbol. --Costo: O(log N)
belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST _ EmptyT          = False
belongsBST x (NodeT y ti td) = if (x == y)
                                then True
                                else if (x < y) 
                                        then belongsBST x ti
                                        else belongsBST x td

--Satisface el costo dado, porque se recorre una sola rama del arbol para encontrarlo.

--2. Propósito: dado un BST inserta un elemento en el árbol. Costo: O(log N)
insertBST :: Ord a => a -> Tree a -> Tree a
insertBST x EmptyT          = NodeT x EmptyT EmptyT
insertBST x (NodeT y ti td) = if (x == y)
                                then (NodeT x ti td)
                                else if (x < y)
                                        then NodeT y (insertBST x ti) td
                                        else NodeT y ti (insertBST x td)

--Satisface el costo pedido, porque se inserta en una sola rama del arbol dado.

--3. Propósito: dado un BST borra un elemento en el árbol. Costo: O(log N)
deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST x EmptyT          = EmptyT
deleteBST x (NodeT y ti td) = if (x == y)
                                then rearmarBST ti td
                                else if (x < y)
                                        then NodeT y (deleteBST x ti) td
                                        else NodeT y ti (deleteBST x td)

rearmarBST :: Ord a => Tree a -> Tree a -> Tree a       --Costo: O(log N), en el peor de los casos, O(N)
--PRECOND: AMBOS ARBOLES DEBE SER BST.
rearmarBST EmptyT td = td
rearmarBST ti td     = NodeT (maxDe ti) (sinElMax ti) td

maxDe :: Ord a => Tree a -> a                           --Costo: O(log N), en el peor de los casos, O(N)
--PRECOND: NO ES VACIO.
maxDe (NodeT x _ EmptyT) = x
maxDe (NodeT x ti td)    = maxDe td

sinElMax :: Ord a => Tree a -> Tree a                   --Costo: O(log N), en el peor de los casos, O(N)
--PRECOND: NO ES VACIO.
sinElMax (NodeT x ti EmptyT) = ti
sinElMax (NodeT x ti td)    = (NodeT x ti (sinElMax td))

--Satisface el costo dado porque para eliminar el elemento pedido, se recorre una parte del arbol con N elementos.

--4. Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo. Costo: O(log N)
splitMinBST :: Ord a => Tree a -> (a, Tree a)
--PRECOND: EL ARBOL NO ES VACIO Y ES BST.
splitMinBST (NodeT x EmptyT td) = (x, td)
splitMinBST (NodeT x ti td)     = let (m, ti') = splitMinBST ti
                                    in (m, NodeT x ti' td)

--

--5. Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo. Costo: O(log N)
splitMaxBST :: Ord a => Tree a -> (a, Tree a)
--PRECOND: EL ARBOL NO ES VACIO Y ES BST.
splitMaxBST (NodeT x ti EmptyT) = (x, ti)
splitMaxBST (NodeT x ti td)     = let (m, td') = splitMaxBST td
                                    in (m, NodeT x ti td')

--6. Propósito: indica si el árbol cumple con los invariantes de BST. Costo: O(N^2)
esBST :: Ord a => Tree a -> Bool
esBST EmptyT = True
esBST (NodeT x ti td) =
  let
    listaIzquierda = treeToList ti
    listaDerecha = treeToList td
    mayorIzq = esMayorQueTodo x listaIzquierda
    menorDer = esMenorQueTodo x listaDerecha
  in
    mayorIzq && menorDer && esBST ti && esBST td

esMayorQueTodo :: Ord a => a -> [a] -> Bool             --Costo: O(n).
esMayorQueTodo _ []     = True
esMayorQueTodo x (y:ys) = x > y && esMayorQueTodo x ys

esMenorQueTodo :: Ord a => a -> [a] -> Bool             --Costo: O(n).
esMenorQueTodo _ []     = True
esMenorQueTodo x (y:ys) = x < y && esMenorQueTodo x ys

treeToList :: Tree a -> [a]                             --Costo: O(n^2), con n representando los elementos de la lista.
treeToList EmptyT = []
treeToList (NodeT x ti td) = treeToList ti ++ [x] ++ treeToList td

--Se consigue el costo esperado, ya que utilizo una concatenacion de listas, lo que conlleva, hasta donde vimos, en un costo de O(n^2), y encima
-- cada elemento del arbol hace se compara con una determinada cantidad de elementos hasta llegar al fin.

--7. Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al elemento dado.                                         
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a       --Costo: O(log N)
--PRECOND: EL ARBOL DADO ES UN BST.
elMaximoMenorA _ EmptyT = Nothing
elMaximoMenorA x (NodeT y ti td) = if (y < x )
                                    then case elMaximoMenorA x td of
                                            Just maxDer -> Just maxDer
                                            Nothing     -> Just y
                                    else elMaximoMenorA x ti

--8. Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al elemento dado. Costo: O(log N)
elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
--PRECOND: EL ARBOL DADO ES UN BST.
elMinimoMayorA _ EmptyT = Nothing
elMinimoMayorA x (NodeT y ti td) = if (y > x )
                                    then case elMinimoMayorA x ti of
                                            Just minDer -> Just minDer
                                            Nothing     -> Just y
                                    else elMinimoMayorA x td

--9. 
--Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
--nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1. Costo: O(N2)
balanceado :: Tree a -> Bool
balanceado EmptyT = True
balanceado (NodeT _ ti td) =
  abs (altura ti - altura td) <= 1 && balanceado ti && balanceado td

altura :: Tree a -> Int
altura EmptyT = 0
altura (NodeT _ ti td) = 1 + max (altura ti) (altura td)

{-
Ejercicio 3.

Costos como usuario de Map:
emptyM :: Map k v
Costo: O(1).
assocM :: Ord k => k -> v -> Map k v -> Map k v
Costo: O(log K).
lookupM :: Ord k => k -> Map k v -> Maybe v
Costo: O(log K).
deleteM :: Ord k => k -> Map k v -> Map k v
Costo: O(log K).
keys :: Map k v -> [k]
Costo: O(K).

funciones como usuario:
1. valuesM :: Eq k => Map k v -> [Maybe v]
Propósito: obtiene los valores asociados a cada clave del map.

COSTO DE valuesM: O(k log k)
valuesM :: Eq k => Map k v -> [Maybe v] -- O(K) + (O(log k) * k)
valuesM m = recorrer m (keys m)

recorrer :: Eq k => Map k v -> [k] -> [Maybe v] -- O(log k) * K
recorrer _ []     = []
recorrer m (k:ks) = lookupM k m : recorrer m ks 

2. todasAsociadas :: Eq k => [k] -> Map k v -> Bool
Propósito: indica si en el map se encuentran todas las claves dadas.

Costo de todasAsociadas = O(K)
todasAsociadas :: Eq k => [k] -> Map k v -> Bool -- O(K)
todasAsociadas [] _     = True
todasAsociadas (k:ks) m = elem k (keys m) && todasAsociadas ks m

3. listToMap :: Eq k => [(k, v)] -> Map k v
Propósito: convierte una lista de pares clave valor en un map.

COSTO = O(L log L)
listToMap :: Eq k => [(k, v)] -> Map k v -- O((1 + log k)*KVS)
listToMap []          = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

4. mapToList :: Eq k => Map k v -> [(k, v)]
Propósito: convierte un map en una lista de pares clave valor.

Costo Final = O(K log K)
mapToList :: Eq k => Map k v -> [(k, v)] 
mapToList m = completarLista m (keys m) O(K)

completarLista :: Eq k => Map k v -> [k] -> [(k, v)]
completarLista _ []      = []
completarLista m (k:ks) = case lookupM k m of  O(K log K)
                            Just v -> (k, v) : (completarLista m ks)
                            Nothing -> completarLista m ks

5. agruparEq :: Eq k => [(k, v)] -> Map k [v]
Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
la misma clave.

Costo final = O(N log N)
agruparEq :: Eq k => [(k, v)] -> Map k [v] -- O(N log N)
agruparEq []           = emptyM O(1)
agruparEq ((k, v):kvs) = 
    let resto = agruparEq kvs 
    in case lookupM k resto of  O(log N)
        Just vs -> assocM k (v : vs) resto
        Nothing -> assocM k [v] resto 

6. incrementar :: Eq k => [k] -> Map k Int -> Map k Int
Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
cada número asociado con dichas claves.

Costo Final = O(L log K)
incrementar :: Eq k => [k] -> Map k Int -> Map k Int -- O(K log K)
incrementar [] _     = emptyM           O(1)
incrementar (k:ks) m =     recorrer por L, donde L es la cantidad de claves en la lista
    case lookupM k m of    O(log k)
        Just v  -> assocM k (v + 1) (incrementar ks m) O(log K)
        Nothing -> incrementar ks m

7. mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
una clave del primero existe en el segundo, es reemplazada por la del primero.

Costo final = O(K log K)
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v -- O(K log K)
mergeMaps m1 m2 = mergeAuxiliar (keys m1) m1 m2   O(K)

mergeAuxiliar :: Eq k => [k] -> Map k v -> Map k v -> Map k v -- O(K log K)
mergeAuxiliar [] _ m2      = m2
mergeAuxiliar (k:ks) m1 m2 =                    K como una lista de claves
    let v = lookupM k m1                        O(log k)
    in case v of
        Just val -> mergeAuxiliar ks m1 (assocM k val m2)       O(log k)
        Nothing  -> mergeAuxiliar ks m1 m2

8. indexar :: [a] -> Map Int a
Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
su posición en la lista.

Costo final = O(N log N)
indexar :: [a] -> Map Int a
indexar []    = emptyM                                  O(1)
indexar lista = indexar' 0 lista                        
  where
    indexar' :: Int -> [a] -> Map Int a
    indexar' _ []     = emptyM                          O(1)
    indexar' i (x:xs) = assocM i x (indexar' (i + 1) xs)        O(N log N)

9. ocurrencias :: String -> Map Char Int
Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
en el string, y los valores la cantidad de veces que aparecen en el mismo.

Costo final = O(K log K)
ocurrencias :: String -> Map Char Int
ocurrencias ""    = emptyM                                                      O(1)
ocurrencias cs = actualizarOcurrencias' cs emptyM
  where
    actualizarOcurrencias' :: String -> Map Char Int -> Map Char Int
    actualizarOcurrencias' [] mapa     = mapa
    actualizarOcurrencias' (c:cs) mapa =
      case lookupM c mapa of                                                    O(log K)
        Nothing -> actualizarOcurrencias' cs (assocM c 1 mapa)                  O(log K)
        Just count -> actualizarOcurrencias' cs (assocM c (count + 1) mapa)     O(log K)
-}

import Empresa

{-
consEmpleado :: CUIL -> Empleado
Propósito: construye un empleado con dicho CUIL.
Costo: O(1)
cuil :: Empleado -> CUIL
Propósito: indica el CUIL de un empleado.
Costo: O(1)
incorporarSector :: SectorId -> Empleado -> Empleado
Propósito: incorpora un sector al conjunto de sectores en los que trabaja un empleado.
Costo: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.
sectores :: Empleado -> [SectorId]
Propósito: indica los sectores en los que el empleado trabaja.
Costo: O(S)
-}

--Propósito: construye una empresa con la información de empleados dada. Los sectores no
--tienen empleados.
--Costo: calcular.    O(S(logS) + E(logE)) donde S es la lista de SectorID y E es la lista de CUILs.
comenzarCon :: [SectorId] -> [CUIL] -> Empresa
comenzarCon sectorIds cuils = ConsE (crearMapSectores sectorIds emptyM) (crearMapCUILs cuils emptyM)

crearMapSectores :: [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado) --O(S*(logS))
crearMapSectores [] mapS = mapS
crearMapSectores (sec:secs) mapS = crearMapSectores secs (assocM sec emptyS mapS)

crearMapCUILs :: [CUIL] -> Map CUIL Empleado -> Map CUIL Empleado                             --O(E*(logE))
crearMapCUILs [] mapC = mapC
crearMapCUILs (c:cs) mapC = crearMapCUILs cs (assocM c (consEmpleado c) mapC)

--Propósito: dada una empresa elimina a la mitad de sus empleados (sin importar a quiénes).
-- Costo: O(E * (S * (log S + log E))), donde E es la cantidad inicial de empleados.
recorteDePersonal :: Empresa -> Empresa
recorteDePersonal empresa = recortar (todosLosCUIL empresa) (length (todosLosCUIL empresa) `div` 2) empresa

recortar :: [CUIL] -> Int -> Empresa -> Empresa
recortar [] _ empresa = empresa
recortar (c:cs) cantidadAEliminar empresa = if cantidadAEliminar <= 0 
                                              then empresa
                                              else recortar cs (cantidadAEliminar - 1)(borrarEmpleado c empresa)

--Propósito: dado un CUIL de empleado le asigna todos los sectores de la empresa.
--Costo: O(S * (logS + logE)).
convertirEnComodin :: CUIL -> Empresa -> Empresa
convertirEnComodin cuil empresa = agregarEmpleado (todosLosSectores empresa) cuil empresa

--Propósito: dado un CUIL de empleado indica si el empleado está en todos los sectores.
-- Costo: O(S^2)
esComodin :: CUIL -> Empresa -> Bool
esComodin cuil empresa =
  case buscarPorCUIL cuil empresa of
    Nothing -> False -- El empleado no existe en la empresa
    Just empleado ->
      let sectoresEmpresa = todosLosSectores empresa
          sectoresEmpleado = sectores empleado
      in all (`elem` sectoresEmpleado) sectoresEmpresa