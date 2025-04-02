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
--PRECOND: TIENE QUE HABER AL MENOS UN TESORO
pasosHastaTesoro Fin            = error "TIENE QUE HABER AL MENOS UN TESORO" 
pasosHastaTesoro (Nada c)       = 1 + (pasosHastaTesoro c)
pasosHastaTesoro (Cofre objs c) = unoSi (hayTesoro' objs) + pasosHastaTesoro c

--c.
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 _              = True
alMenosNTesoros _ Fin            = False
alMenosNTesoros n (Nada c)       = alMenosNTesoros n c
alMenosNTesoros n (Cofre objs c) = if ( hayTesoro' objs )
                                    then alMenosNTesoros (n-1) c
                                    else alMenosNTesoros n c 

--d.
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre 0  n2 c = contarTesorosHasta n2 c
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