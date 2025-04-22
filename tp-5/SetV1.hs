module SetV1
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
    where

data Set a = S [a] Int
    {-INV. REP:
        * no tiene elementos repetidos.
        * Int representa la cantidad de elementos en la lista
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
setToList (S xs n)       = xs

agregar :: Eq a => a -> Set a -> Set a
agregar a set = if belongs a set
                then set
                else (S (a : setToList set) (incrementarNumeroDe set))

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece a (x:xs) = a == x || pertenece a xs

sacarElemento :: Eq a => a -> [a] -> Int -> Set a
sacarElemento _ [] _     = (S [] 0)
sacarElemento a (x:xs) n = if a == x
                            then (S xs (n-1))
                            else (sacarElemento a xs n)

unificarListas :: Eq a => [a] -> [a] -> Set a
unificarListas [] ys     = S ys (length ys)
unificarListas (x:xs) ys = addS x (unificarListas xs ys)

incrementarNumeroDe :: Set a -> Int
incrementarNumeroDe (S xs n) = (n+1)

--agregar (head' xs) ys (length ys)
--S (quitarElemento a xs) (n - unoSi (pertenece a xs))
--S (unificarListas xs ys) (length (unificarListas xs ys))
--S (agregarSinRepetir a xs) (n + unoSi (pertenece a xs))