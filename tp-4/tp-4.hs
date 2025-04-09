--EJERCICIO 1 (PIZZAS)

data Pizza = Prepizza| Capa Ingrediente Pizza
    deriving Show
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
    deriving Show

pizzaNormal :: Pizza
pizzaNormal = (Capa Queso (Capa Salsa (Prepizza)))

--Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa _ p) = 1 +  (cantidadDeCapas p)

---------------------------------------------------------------------------------------

--Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza []     = Prepizza
armarPizza (i:is) = (Capa i (armarPizza is))

---------------------------------------------------------------------------------------

--Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza   = Prepizza
sacarJamon (Capa i p) = if (esJamon i)
                        then sacarJamon p
                        else (Capa i (sacarJamon p))

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False

{-
case p of 
    (Capa Jamon p) -> sacarJamon p
    (Capa i p)     -> (Capa i (sacarJamon p))
-}

--Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En
--particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)

---------------------------------------------------------------------------------------

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza       = True
tieneSoloSalsaYQueso p = case p of
                            (Capa Queso (Capa Salsa Prepizza)) -> True
                            (Capa i p)                         -> False

---------------------------------------------------------------------------------------
--Recorre cada ingrediente y si es aceitunas duplica su cantidad.
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas p        = case p of
                                (Capa (Aceitunas n) p) -> (Capa (Aceitunas (n*2)) (duplicarAceitunas p))
                                (Capa i p)           -> (Capa i (duplicarAceitunas p)) 

---------------------------------------------------------------------------------------

--Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
--ingredientes de la pizza, y la respectiva pizza como segunda componente.                                
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) = ((cantidadDeCapas p), p) : cantCapasPorPizza ps

---------------------------------------------------------------------------------------

--EJERCICIO 2 MAPA DE TESOROS CON BIFURCACIONES

data Dir = Izq | Der
    deriving Show
data Objeto = Tesoro | Chatarra
    deriving Show
data Cofre = Cofre [Objeto]
    deriving Show
data Mapa = Fin Cofre| Bifurcacion Cofre Mapa Mapa
    deriving Show

mapaConTesoroCorto :: Mapa
mapaConTesoroCorto = (Bifurcacion (Cofre [Chatarra]) (Fin (Cofre [Tesoro])) (Fin (Cofre [Chatarra])))

--1. Indica si hay un tesoro en alguna parte del mapa.
hayTesoro :: Mapa -> Bool
hayTesoro (Fin c)               = hayTesoroEnCofre c
hayTesoro (Bifurcacion c mi md) = hayTesoroEnCofre c || hayTesoro mi || hayTesoro md

hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (Cofre objs) = hayTesoroEnObjs objs

hayTesoroEnObjs :: [Objeto] -> Bool
hayTesoroEnObjs []     = False
hayTesoroEnObjs (x:xs) = esTesoro x || hayTesoroEnObjs xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

-------------------------------------------------------------------------------------------------------------

--2. Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una lista vacía de direcciones.

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Fin c)                   = hayTesoroEnCofre c
hayTesoroEn [] (Bifurcacion c mi md)     = hayTesoroEnCofre c
hayTesoroEn ds (Fin _)                   = False
hayTesoroEn (d:ds) (Bifurcacion _ mi md) = hayTesoroEn ds (siguienteCaminoHacia d mi md)

siguienteCaminoHacia :: Dir -> Mapa -> Mapa -> Mapa
siguienteCaminoHacia Izq mi md = mi
siguienteCaminoHacia Der mi md = md

-------------------------------------------------------------------------------------------------------------

--3. Indica el camino al tesoro. Precondición: existe un tesoro y es único
caminoAlTesoro :: Mapa -> [Dir]
--PRECOND: EXISTE UN TESORO Y ES UNICO.
caminoAlTesoro (Fin _)               = []
caminoAlTesoro (Bifurcacion c mi md) = if (hayTesoroEnCofre c)
                                            then []
                                            else (elegirCaminoConTesoro mi md) : caminoAlTesoro (caminoConTesoro mi md)

elegirCaminoConTesoro :: Mapa -> Mapa -> Dir
--PRECOND: EXISTE UN TESORO.
elegirCaminoConTesoro mi md = if (hayTesoro mi)
                                then Izq
                                else Der

caminoConTesoro :: Mapa -> Mapa -> Mapa
--PRECOND: EXISTE UN TESORO.
caminoConTesoro mi md = if (hayTesoro mi)
                                then mi
                                else md

-------------------------------------------------------------------------------------------------------------

--4. Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin _)               = []
caminoDeLaRamaMasLarga (Bifurcacion _ mi md) = if (heightM mi > heightM md)
                                                    then Izq : caminoDeLaRamaMasLarga mi
                                                    else Der : caminoDeLaRamaMasLarga md


heightM :: Mapa -> Int
heightM (Fin _)          = 0
heightM (Bifurcacion _ m1 m2) = 1 + max (heightM m1) (heightM m2)

-------------------------------------------------------------------------------------------------------------

--5. Devuelve los tesoros separados por nivel en el árbol
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c)               = [tesorosDe c]
tesorosPorNivel (Bifurcacion c mi md) = tesorosDe c : unirTesoros (tesorosPorNivel mi) (tesorosPorNivel md)

tesorosDe :: Cofre -> [Objeto]
tesorosDe (Cofre [])     = []
tesorosDe (Cofre (t:ts)) = if esTesoro t 
                        then t : tesorosDe (Cofre ts)
                        else tesorosDe (Cofre ts) 

unirTesoros :: [[Objeto]] -> [[Objeto]] -> [[Objeto]]
unirTesoros [] t2s            = t2s
unirTesoros t1s []            = t1s
unirTesoros (t1s:t1ss) (t2s:t2ss) = (t1s ++ t2s) : unirTesoros t1ss t2ss 

unirNiveles :: [[a]] -> [[a]] -> [[a]]
unirNiveles [] yss         = yss
unirNiveles xss []         = xss
unirNiveles (xs:xss) (ys:yss) = (xs ++ ys) : unirNiveles xss yss