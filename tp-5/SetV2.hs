module SetV2
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
    where

data Set a = S [a] Int
    {-INV. REP:
        * puede tener elementos repetidos, pero para el usuario son sin repetir.
        * Int representa la cantidad de elementos no repetidos de la lista.
        -}

emptyS :: Set a
addS :: Eq a => a -> Set a -> Set a
belongs :: Eq a => a -> Set a -> Bool
sizeS :: Eq a => Set a -> Int
removeS :: Eq a => a -> Set a -> Set a
unionS :: Eq a => Set a -> Set a -> Set a
setToList :: Eq a => Set a -> [a]

emptyS                   = (S [] 0)
addS a set               = agregar a set
belongs a (S xs n)       = pertenece a xs
sizeS (S xs n)           = n
removeS a (S xs n)       = sacarElemento a xs n
unionS (S xs n) (S ys m) = unificarListas xs ys
setToList (S xs n)       = elementosSinRepetir xs

agregar :: Eq a => a -> Set a -> Set a
agregar a set = if belongs a set
                then (S (a : setToList set) (numerdoDe set))
                else (S (a : setToList set) (incrementarNumeroDe set))

numerdoDe :: Set a -> Int
numerdoDe (S _ n) = n

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece a (x:xs) = a == x || pertenece a xs

elementosSinRepetir :: Eq a => [a] -> [a]
elementosSinRepetir []     = []
elementosSinRepetir (x:xs) = if (pertenece x xs)
                                then elementosSinRepetir xs
                                else x : (elementosSinRepetir xs)

sacarElemento :: Eq a => a -> [a] -> Int -> Set a
sacarElemento _ [] _     = (S [] 0)
sacarElemento a (x:xs) n = if a == x
                            then (sacarElemento a xs (n-1))
                            else addS x (sacarElemento a xs n)

unificarListas :: Eq a => [a] -> [a] -> Set a
unificarListas [] ys     = S ys (length ys)
unificarListas (x:xs) ys = if pertenece x ys
                                then (unificarListas xs ys)
                                else addS x (unificarListas xs ys)

incrementarNumeroDe :: Set a -> Int
incrementarNumeroDe (S xs n) = (n+1)