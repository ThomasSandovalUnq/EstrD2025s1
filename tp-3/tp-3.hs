--Ejercicio 1

--1.1.

data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

celda1 :: Celda
celda1 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
celda2 :: Celda
celda2 = Bolita Rojo CeldaVacia

--a.
nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia         = 0
nroBolitas color (Bolita cr ca) = (unoSi (sonElMismoColor color cr)) + (nroBolitas color ca)

sonElMismoColor :: Color -> Color -> Bool
sonElMismoColor Azul Azul = True
sonElMismoColor Rojo Rojo = True
sonElMismoColor _    _    = False

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

--b.
poner :: Color -> Celda -> Celda
poner color CeldaVacia = (Bolita color CeldaVacia)
poner color bolitaO    = (Bolita color bolitaO)

--c.
sacar :: Color -> Celda -> Celda
sacar color CeldaVacia     = CeldaVacia
sacar color (Bolita cr ca) = if (sonElMismoColor color cr)
                            then ca
                            else Bolita cr (sacar color ca)

--d.
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 color celda = celda
ponerN n color celda = (poner color (ponerN (n-1) color celda) )

--1.2

data Objeto = Cacharro | Tesoro
    deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show

caminoCorto :: Camino
caminoCorto = Cofre [Tesoro, Cacharro] Fin
caminoCorto' :: Camino
caminoCorto' = Cofre [Cacharro] Fin

caminoMedio :: Camino
caminoMedio = Nada ( Nada ( Cofre [Cacharro] Fin ) )
caminoMedio' :: Camino
caminoMedio' = Nada ( Nada ( Cofre [Tesoro,Cacharro] (Cofre [Tesoro] (Nada Fin) ) ))


--a.
hayTesoro :: Camino -> Bool
hayTesoro Fin                    = False  
hayTesoro (Nada camino)          = hayTesoro camino
hayTesoro (Cofre objetos camino) = hayTesoro' objetos || hayTesoro camino

hayTesoro' :: [Objeto] -> Bool
hayTesoro' []     = False
hayTesoro' (x:xs) = esTesoro x || hayTesoro' xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

--b.

pasosHastaTesoro :: Camino -> Int
--PRECND: TIENE QUE HABER AL MENOS UN TESORO
pasosHastaTesoro Fin            = error "TIENE QUE HABER AL MENOS UN TESORO" 
pasosHastaTesoro (Nada c)       = 1 + (pasosHastaTesoro c)
pasosHastaTesoro (Cofre objs c) = if hayTesoro' objs
                                    then 0
                                    else 1 + pasosHastaTesoro c

--c. Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
--pasos es 5, indica si hay un tesoro en 5 pasos

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n (Fin)          = False
hayTesoroEn n (Nada c)       = hayTesoroEn (n-1) c
hayTesoroEn n (Cofre objs c) = if n == 0
                                then hayTesoro' objs
                                else hayTesoroEn (n-1) c
 
--d.
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 _              = True
alMenosNTesoros _ Fin            = False
alMenosNTesoros n (Nada c)       = alMenosNTesoros n c
alMenosNTesoros n (Cofre objs c) = alMenosNTesoros (n-(contarTesoros objs)) c

contarTesoros :: [Objeto] -> Int
contarTesoros [] = 0
contarTesoros (t:ts) = unoSi(esTesoro t) + contarTesoros ts
contarTesoros (_:ts) = contarTesoros ts                                    

--e.
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre 0  n2 c = contarTesorosHasta n2 c
cantTesorosEntre _ _ Fin = 0
cantTesorosEntre n1 n2 c = cantTesorosEntre (n1-1) (n2-1) (siguienteCaminoDe c)

siguienteCaminoDe :: Camino -> Camino
siguienteCaminoDe Fin         = error "NO HAY MAS CAMINO D:"
siguienteCaminoDe (Nada c)    = c
siguienteCaminoDe (Cofre _ c) = c

contarTesorosHasta :: Int -> Camino -> Int
contarTesorosHasta n Fin            = 0
contarTesorosHasta n (Nada c)       = (contarTesorosHasta (n-1) c)
contarTesorosHasta 0 (Cofre objs c) = unoSi (hayTesoro' objs)
contarTesorosHasta n (Cofre objs c) = unoSi (hayTesoro' objs) + contarTesorosHasta (n-1) c

----------------------------------------------------------------------------------------------------

--Ejercicio 2

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

arbolHasta5 :: Tree Int
arbolHasta5 = (NodeT 1 (NodeT 2 (NodeT 3 (EmptyT) (EmptyT)) (NodeT 4 (NodeT 6 (EmptyT) (EmptyT)) (EmptyT))) (NodeT 5 (EmptyT) (EmptyT)))

arbolVocales :: Tree Char
arbolVocales = (NodeT 'a' (NodeT 'e' (NodeT 'i' (EmptyT) (EmptyT)) (NodeT 'o' (EmptyT) (EmptyT))) (NodeT 'u' (EmptyT) (EmptyT)))

--2.1.1.
sumarT :: Tree Int -> Int
sumarT EmptyT          = 0
sumarT (NodeT x t1 t2) = x + (sumarT t1) + (sumarT t2)

--2.1.2.
sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT _ t1 t2) = 1 + (sizeT t1) + (sizeT t2)

--2.1.3.
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT n t1 t2) = (NodeT (n*2) (mapDobleT t1) (mapDobleT t2) )

--2.1.4.
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT a EmptyT          = False
perteneceT a (NodeT x t1 t2) = (a == x) || (perteneceT a t1) || (perteneceT a t2)

--2.1.5.
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT a EmptyT          = 0
aparicionesT a (NodeT x t1 t2) = unoSi (a == x) 
                                + (aparicionesT a t1) 
                                + (aparicionesT a t2)

--2.1.6.
leaves :: Tree a -> [a]
leaves EmptyT          = []
leaves (NodeT x EmptyT EmptyT) = x:[]
leaves (NodeT x t1 t2) = (leaves t1) ++ (leaves t2)

--2.1.7.
heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT _ t1 t2) = 1 + max (heightT t1) (heightT t2)

--2.1.8.
mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT a t1 t2) = (NodeT a (mirrorT t2) (mirrorT t1) )

--2.1.9.
toList :: Tree a -> [a]
toList EmptyT          = []
toList (NodeT a t1 t2) = toList t1 ++ [a] ++ toList t2

--2.1.10.
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT        = []
levelN 0 (NodeT a _ _) = [a]  
levelN n (NodeT a t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

--2.1.11.
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT a t1 t2) = [a] : unirNiveles (listPerLevel t1) (listPerLevel t2)

unirNiveles :: [[a]] -> [[a]] -> [[a]]
unirNiveles [] yss         = yss
unirNiveles xss []         = xss
unirNiveles (xs:xss) (ys:yss) = (xs ++ ys) : unirNiveles xss yss

--2.1.12.
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT a t1 t2) = a : (ramaMasLargaEntre (ramaMasLarga t1)  (ramaMasLarga t2))

ramaMasLargaEntre :: [a] -> [a] -> [a]
ramaMasLargaEntre [] ys = ys
ramaMasLargaEntre xs [] = xs
ramaMasLargaEntre xs ys = if (length xs > length ys)
                            then xs
                            else ys

--2.1.13.
--Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raíz hasta cualquiera de los nodos.
--ATENCIÓN: se trata de todos los caminos, y no solamente de los maximales (o sea, de la raíz hasta la hoja)                            
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT a t1 t2) = [a] : agregarACadaCamino a ((todosLosCaminos t1) ++ (todosLosCaminos t2))

agregarACadaCamino :: a -> [[a]] -> [[a]]
agregarACadaCamino _ []       = []
agregarACadaCamino a (xs:xss) = (a : xs) : (agregarACadaCamino a xss)

todosLosCaminosMaximal :: Tree a -> [[a]]
todosLosCaminosMaximal EmptyT                  = []
todosLosCaminosMaximal (NodeT a EmptyT EmptyT) = [[a]]
todosLosCaminosMaximal (NodeT a t1 t2)         = agregarACadaCamino a ((todosLosCaminosMaximal t1) ++ (todosLosCaminosMaximal t2))

--EJERCICIO 2.2
data ExpA = Valor Int| Sum ExpA ExpA| Prod ExpA ExpA| Neg ExpA
    deriving Show

--2.2.1. Dada una expresión aritmética devuelve el resultado evaluarla.
eval :: ExpA -> Int
eval (Valor n)      = n
eval (Neg ex1)      = -(eval ex1)
eval (Sum ex1 ex2)  = eval ex1 + eval ex2
eval (Prod ex1 ex2) = eval ex1 * eval ex2

{-2.2.2. Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando notación matemática convencional):
a) 0 + x = x + 0 = x
b) 0 * x = x * 0 = 0
c) 1 * x = x * 1 = x
d) - (- x) = x
-}

simplificar :: ExpA -> ExpA
simplificar (Valor n)             = (Valor n)
simplificar (Neg (Neg (exp1)))    = simplificar exp1
simplificar (Neg exp1)            = Neg (simplificar exp1)
simplificar (Prod (Valor 0) exp2) = (Valor 0) 
simplificar (Prod exp1 (Valor 0)) = (Valor 0) 
simplificar (Prod (Valor 1) exp2) = simplificar exp2
simplificar (Prod exp1 (Valor 1)) = simplificar exp1
simplificar (Prod exp1 exp2)      = Prod (simplificar exp1) (simplificar exp2)
simplificar (Sum (Valor 0) exp2)  = simplificar exp2
simplificar (Sum exp1 (Valor 0))  = simplificar exp1
simplificar (Sum exp1 exp2)       = Sum (simplificar exp1) (simplificar exp2)