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

mapaConTesoroMediano :: Mapa
mapaConTesoroMediano = (Bifurcacion (Cofre [])
                            (Fin (Cofre []))
                            (mapaConTesoroCorto)
                        )

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

{- La funcion de unirTesoros, use como referencia el ejercicio de listPerLevel de la practica anterior.
unirNiveles :: [[a]] -> [[a]] -> [[a]]
unirNiveles [] yss         = yss
unirNiveles xss []         = xss
unirNiveles (xs:xss) (ys:yss) = (xs ++ ys) : unirNiveles xss yss
-}

-------------------------------------------------------------------------------------------------------------

--6. Devuelve todos lo caminos en el mapa.
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _)               = [[]]
todosLosCaminos (Bifurcacion _ mi md)      = agregarACadaCamino Izq (todosLosCaminos mi) 
                                            ++ 
                                             agregarACadaCamino Der (todosLosCaminos md)

agregarACadaCamino :: a -> [[a]] -> [[a]]
agregarACadaCamino _ []       = []
agregarACadaCamino a (xs:xss) = (a : xs) : (agregarACadaCamino a xss)

-------------------------------------------------------------------------------------------------------------

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show

data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show

data Sector = S SectorId [Componente] [Tripulante]
    deriving Show

type SectorId = String

type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

data Nave = N (Tree Sector)
    deriving Show

tripulanteJoker :: Tripulante
tripulanteJoker = "Joker"

tripulanteFidel :: Tripulante
tripulanteFidel = "Fidel"

almacenConComida :: Componente
almacenConComida = (Almacen [Comida, Comida])

almacenConCombustible :: Componente
almacenConCombustible = (Almacen [Oxigeno])

naveChiquita :: Nave
naveChiquita = (N (EmptyT))

naveChiquita' :: Nave
naveChiquita' = (N (NodeT (S "A1" [almacenConComida, Motor 94] [tripulanteJoker, tripulanteFidel]) 
                        (EmptyT)
                        (NodeT (S "B3" [almacenConCombustible, Motor 84] [tripulanteFidel, tripulanteJoker]) 
                            (EmptyT)
                            (EmptyT))))

--1. Propósito: Devuelve todos los sectores de la nave
sectores :: Nave -> [SectorId]
sectores (N ss) = todosLosIdsSectoresDe ss 

todosLosIdsSectoresDe ::  Tree Sector -> [SectorId]
todosLosIdsSectoresDe (EmptyT)        = []
todosLosIdsSectoresDe (NodeT s t1 t2) = idDeSector s : (todosLosIdsSectoresDe t1 ++ todosLosIdsSectoresDe t2)

idDeSector :: Sector -> String
idDeSector (S id cs ts) = id

-------------------------------------------------------------------------------------------------------------
 
--2.Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. 
--Nota: el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion :: Nave -> Int
poderDePropulsion (N ss) = sumaDePoderDePropulsionEn ss

sumaDePoderDePropulsionEn :: Tree Sector -> Int
sumaDePoderDePropulsionEn (EmptyT)        = 0
sumaDePoderDePropulsionEn (NodeT s t1 t2) = (poderDeMotorDelSector s) + (sumaDePoderDePropulsionEn t1) + (sumaDePoderDePropulsionEn t2)

poderDeMotorDelSector :: Sector -> Int
poderDeMotorDelSector (S _ componentes _) = poderDePropulsionEn componentes

poderDePropulsionEn :: [Componente] -> Int
poderDePropulsionEn []     = 0
poderDePropulsionEn (c:cs) = if (esMotor c)
                                then poderDeMotor c
                                else poderDePropulsionEn cs

poderDeMotor :: Componente -> Int
--PRECOND: EL COMPONENTE DEBE SER UN MOTOR
poderDeMotor (Motor n) = n

esMotor :: Componente -> Bool
esMotor (Motor _) = True
esMotor _         = False

-------------------------------------------------------------------------------------------------------------

--3. Propósito: Devuelve todos los barriles de la nave
barriles :: Nave -> [Barril]
barriles (N ss) = todosLosBarrilesDe ss

todosLosBarrilesDe :: Tree Sector -> [Barril]
todosLosBarrilesDe (EmptyT)        = []
todosLosBarrilesDe (NodeT s t1 t2) = barrilesDeSector s ++ (todosLosBarrilesDe t1 ++ todosLosBarrilesDe t2)

barrilesDeSector :: Sector -> [Barril]
barrilesDeSector (S _ componentes _) = barrilesDeLosComponentes componentes

barrilesDeLosComponentes :: [Componente] -> [Barril]
barrilesDeLosComponentes []     = []
barrilesDeLosComponentes (c:cs) = if esAlmacen c 
                                        then barrilesEnAlmacen c
                                        else barrilesDeLosComponentes cs

barrilesEnAlmacen :: Componente -> [Barril]
--PRECOND: EL COMPONENTE DEBE SER UN BARRIL
barrilesEnAlmacen (Almacen barriles) = barriles

esAlmacen :: Componente -> Bool
esAlmacen (Almacen _) = True
esAlmacen _           = False

-------------------------------------------------------------------------------------------------------------

--4. Propósito: Añade una lista de componentes a un sector de la nave.
--Nota: ese sector puede no existir, en cuyo caso no añade componentes
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector comps sid (N ss) = N (agregarComponentesEnSector comps sid ss)

agregarComponentesEnSector :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarComponentesEnSector cs sid (EmptyT)        = (EmptyT)
agregarComponentesEnSector cs sid (NodeT s t1 t2) = if (idDeSector s) == sid
                                                        then (NodeT (agregarComponentesASector cs s) 
                                                                (agregarComponentesEnSector cs sid t1)
                                                                (agregarComponentesEnSector cs sid t2)) 
                                                        else (NodeT s 
                                                            (agregarComponentesEnSector cs sid t1)
                                                            (agregarComponentesEnSector cs sid t2))

agregarComponentesASector :: [Componente] -> Sector -> Sector
agregarComponentesASector xs (S id comps trip) = (S id (comps ++ xs) trip) 

-------------------------------------------------------------------------------------------------------------

--5. Propósito: Incorpora un tripulante a una lista de sectores de la nave.
--Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
--PRECOND: TODOS LOS IDS DE LA LISTA EXISTEN EN LA NAVE.
asignarTripulanteA trip idSectores (N ss) = (N (asignarTripulanteEn trip idSectores ss))

asignarTripulanteEn :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteEn trip [] t                   = t
asignarTripulanteEn _ _ (EmptyT)                = EmptyT
asignarTripulanteEn trip xs (NodeT s t1 t2) = (NodeT (asignarTripulante trip xs s) 
                                                    (asignarTripulanteEn trip xs t1) 
                                                    (asignarTripulanteEn trip xs t2))

asignarTripulante :: Tripulante -> [SectorId] -> Sector -> Sector
asignarTripulante trip xs (S id comps trps) = if pertenece id xs
                                            then (S id comps (trip:trps))
                                            else (S id comps trps)

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []     = False
pertenece a (x:xs) = (a == x) || (pertenece a xs)

-------------------------------------------------------------------------------------------------------------

--6. Propósito: Devuelve los sectores en donde aparece un tripulante dado.
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados trip (N ss) = sectoresAsignadosDelTripulante trip ss

sectoresAsignadosDelTripulante :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosDelTripulante _ EmptyT             = []
sectoresAsignadosDelTripulante trip (NodeT s t1 t2) = if (estaElTripulante trip s)
                                                        then (idDeSector s) : (
                                                            (sectoresAsignadosDelTripulante trip t1)
                                                            ++
                                                            (sectoresAsignadosDelTripulante trip t2)
                                                        )
                                                        else (sectoresAsignadosDelTripulante trip t1)
                                                              ++
                                                              (sectoresAsignadosDelTripulante trip t2)

estaElTripulante :: Tripulante -> Sector -> Bool
estaElTripulante trip (S id _ ts) = pertenece trip ts

-------------------------------------------------------------------------------------------------------------

--7.Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
tripulantes :: Nave -> [Tripulante]
tripulantes (N ss) = tripulantesEn ss

tripulantesEn :: Tree Sector -> [Tripulante]
tripulantesEn EmptyT          = []
tripulantesEn (NodeT s t1 t2) = agregarSinRepetir (tripulantesDelSector s) (tripulantesEn t1 ++ tripulantesEn t2)

tripulantesDelSector :: Sector -> [Tripulante]
tripulantesDelSector (S _ _ trip) = trip

agregarSinRepetir :: Eq a => [a] -> [a] -> [a]
agregarSinRepetir [] ys       = ys
agregarSinRepetir xs []       = xs
agregarSinRepetir (x:xs) (ys) = if pertenece x ys
                                    then agregarSinRepetir xs ys
                                    else x : agregarSinRepetir xs ys

-------------------------------------------------------------------------------------------------------------

type Presa = String -- nombre de presa

type Territorio = String -- nombre de territorio

type Nombre = String -- nombre de lobo

data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
    deriving Show

data Manada = M Lobo
    deriving Show

{-1. Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean
crías. Resolver las siguientes funciones utilizando recursión estructural sobre la estructura
que corresponda en cada caso:-}

manadaP :: Manada
manadaP = (M cazadorFidel)

cazadorFidel :: Lobo
cazadorFidel = (Cazador "Fidel" ["Sombra", "Rayo","Sombra", "Rayo","Sombra", "Rayo"] (exploradorJoker) (exploradorYu) (Cria "Thomy"))

exploradorJoker :: Lobo
exploradorJoker = (Explorador "Joker" ["VDP", "La Capilla", "Metaverso"] (Cria "Makoto") (Cria "Sumire"))

exploradorYu :: Lobo
exploradorYu = (Explorador "Yu Narukami" ["VDP", "Heaven", "TV"] (Cria "Rise") (Cria "Yukiko"))

cazadorMessi :: Lobo
cazadorMessi = (Cazador "Messi" ["Sombra", "Real Madrid"] (cazadorFidelio) (cazadorFidel) (Cria "Thiago"))

cazadorFidelio :: Lobo
cazadorFidelio = (Cazador "Fidelio" ["Rayo"] (Cria "Junah") (Cria "Pedro") (Cria "Lil"))

manadaP' :: Manada
manadaP' = (M cazadorMessi)

-------------------------------------------------------------------------------------------------------------

--2. Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.
buenaCaza :: Manada -> Bool
buenaCaza (M lbs) = (cantidadDeCazaDe lbs) > (cantidadDeCriasEn lbs) 

cantidadDeCriasEn :: Lobo -> Int
cantidadDeCriasEn (Cria _)               = 1
cantidadDeCriasEn (Explorador _ _ l1 l2)   = (cantidadDeCriasEn l1) + (cantidadDeCriasEn l2)
cantidadDeCriasEn (Cazador _ _ l1 l2 l3) = (cantidadDeCriasEn l1) + (cantidadDeCriasEn l2) + (cantidadDeCriasEn l3)

cantidadDeCazaDe :: Lobo -> Int
cantidadDeCazaDe (Cria _)                = 0
cantidadDeCazaDe (Explorador _  _ l1 l2)    = (cantidadDeCazaDe l1) + (cantidadDeCazaDe l2)
cantidadDeCazaDe (Cazador _ ps l1 l2 l3) = (longitud ps) + (cantidadDeCazaDe l1) + (cantidadDeCazaDe l2) + (cantidadDeCazaDe l3)

longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

-------------------------------------------------------------------------------------------------------------

--3. Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
--con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
--cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
--cero presas.

elAlfa :: Manada -> (Nombre, Int)
elAlfa (M lbs) = elLoboMasCazador lbs

elLoboMasCazador :: Lobo -> (Nombre, Int)
elLoboMasCazador (Cria n) = (n, 0)
elLoboMasCazador (Explorador n _ l1 l2) = elMasCazadorEntre (n,0) (elMasCazadorEntre (elLoboMasCazador l1) (elLoboMasCazador l2))
elLoboMasCazador (Cazador n ps l1 l2 l3) = elMasCazadorEntre (n,(longitud ps)) (elMasCazadorEntre(elMasCazadorEntre (elLoboMasCazador l1) 
                                                                                                                    (elLoboMasCazador l2))
                                                                                    (elLoboMasCazador l3))

elMasCazadorEntre :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elMasCazadorEntre (n1, x) (n2, y) = if x > y
                                        then (n1, x)
                                        else (n2, y)

-------------------------------------------------------------------------------------------------------------

--4.Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que pasaron por dicho territorio

losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron ter (M lbs) = lobosQueExploraron ter lbs

lobosQueExploraron :: Territorio -> Lobo -> [Nombre]
lobosQueExploraron _ (Cria _)                = []
lobosQueExploraron t (Cazador _ _ l1 l2 l3)  = lobosQueExploraron t l1 ++  lobosQueExploraron t l2 ++ lobosQueExploraron t l3
lobosQueExploraron t (Explorador n ts l1 l2) = exploroElLobo t ts n  ++ (lobosQueExploraron t l1 ++  lobosQueExploraron t l2)

exploroElLobo :: Territorio -> [Territorio] -> Nombre -> [Nombre]
exploroElLobo t ts n = if (pertenece t ts)
                        then [n]
                        else []

-------------------------------------------------------------------------------------------------------------

--5. Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un territorio y 
--cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron dicho territorio. 
--Los territorios no deben repetirse.

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M lbs) = territoriosExplorados lbs

territoriosExplorados :: Lobo -> [(Territorio, [Nombre])]
territoriosExplorados (Cria _)                = []
territoriosExplorados (Cazador _ _ l1 l2 l3)  = fusionarTerritorios
                                                 (territoriosExplorados l1)
                                                 (territoriosExplorados l2)
                                                 (territoriosExplorados l3)
territoriosExplorados (Explorador n ts l1 l2) = fusionarTerritorios
                                                 (territoriosExploradosPor n ts)
                                                 (territoriosExplorados l1)
                                                 (territoriosExplorados l2)

territoriosExploradosPor :: Nombre -> [Territorio] -> [(Territorio, [Nombre])]
territoriosExploradosPor _ []     = []
territoriosExploradosPor n (t:ts) = (t, [n]) : territoriosExploradosPor n ts

fusionarTerritorios :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
fusionarTerritorios xs ys zs = fusionar xs (fusionar ys zs)

fusionar :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
fusionar [] ys     = ys
fusionar (x:xs) ys = fusionar xs (combinarNombres x ys)

combinarNombres :: (Territorio, [Nombre]) -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
combinarNombres tn []     = [tn]
combinarNombres tn (x:xs) = if territorio tn == territorio x
                                then (territorio tn, (nombresDe tn) ++ (nombresDe x)) : xs
                                else x : combinarNombres tn xs

territorio :: (Territorio, [Nombre]) -> Territorio
territorio (t, ns) = t

nombresDe :: (Territorio, [Nombre]) -> [Nombre]
nombresDe (t, ns) = ns

-------------------------------------------------------------------------------------------------------------

--6. Propósito: dado el nombre de un lobo y una manada, indica el nombre de todos los cazadores que tienen como subordinado al lobo dado 
--(puede ser un subordinado directo, o el subordinado de un subordinado).
--Precondición: hay un lobo con dicho nombre y es único.

cazadoresSuperioresDe :: Nombre -> Manada -> [Nombre]
--PRECOND: HAY UN LOBO CON DICHO NOMBRE Y ES UNICO.
cazadoresSuperioresDe n (M lobo) = cazadoresSuperioresA n lobo

cazadoresSuperioresA :: Nombre -> Lobo -> [Nombre]
cazadoresSuperioresA n (Cria nombre)               = if n == nombre
                                                        then []
                                                        else []
cazadoresSuperioresA n (Explorador nombre _ l1 l2) = if n == nombre
                                                        then []
                                                        else cazadoresSuperioresPara n l1 l2
cazadoresSuperioresA n (Cazador nombre _ l1 l2 l3) = if n == nombre
                                                        then []
                                                        else (cazadoresSuperioresEntre n nombre l1 l2 l3)

estaElLobo :: Nombre -> Lobo -> Bool
estaElLobo n (Cria nombre)               = n == nombre
estaElLobo n (Explorador nombre _ l1 l2) = n == nombre || estaElLobo n l1 || estaElLobo n l2
estaElLobo n (Cazador nombre _ l1 l2 l3) = n == nombre || estaElLobo n l1 || estaElLobo n l2 || estaElLobo n l3

cazadoresSuperioresPara :: Nombre -> Lobo -> Lobo -> [Nombre]
cazadoresSuperioresPara n l1 l2 = if estaElLobo n l1 || estaElLobo n l2
                                    then (cazadoresSuperioresA n l1) ++ (cazadoresSuperioresA n l2)
                                    else []

cazadoresSuperioresEntre :: Nombre -> Nombre -> Lobo -> Lobo -> Lobo -> [Nombre]
cazadoresSuperioresEntre n nombre l1 l2 l3 = if estaElLobo n l1 || estaElLobo n l2 || estaElLobo n l3
                                                then nombre : (cazadoresSuperioresA n l1 ++ 
                                                                cazadoresSuperioresA n l2 ++ 
                                                                cazadoresSuperioresA n l3)
                                                 else (cazadoresSuperioresA n l1 
                                                        ++ cazadoresSuperioresA n l2 
                                                        ++ cazadoresSuperioresA n l3)