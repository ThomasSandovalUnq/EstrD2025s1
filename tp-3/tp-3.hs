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