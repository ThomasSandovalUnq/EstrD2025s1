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

--a.
hayTesoro :: Camino -> Bool
hayTesoro Fin                    = False  
hayTesoro (Nada camino)          = hayTesoro camino
hayTesoro (Cofre objetos camino) = if (hayTesoroEn objetos)
                                    then True
                                    else hayTesoro camino

hayTesoroEn :: [Objeto] -> Bool
hayTesoroEn []     = False
hayTesoroEn (x:xs) = if (esTesoro x)
                        then True
                        else hayTesoroEn xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

--b.
pasosHastaTesoro :: Camino -> Int
--PRECOND: TIENE QUE HABER AL MENOS UN TESORO
pasosHastaTesoro Fin            = error "TIENE QUE HABER AL MENOS UN TESORO" 
pasosHastaTesoro (Nada c)       = 1 + (pasosHastaTesoro c)
pasosHastaTesoro (Cofre objs c) = unoSi (not (hayTesoroEn objs)) + (pasosHastaTesoro c)