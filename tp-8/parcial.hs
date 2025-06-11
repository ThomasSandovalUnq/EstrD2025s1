--Parcial Busqueda y Filtro

--EJERCICIO 1
siguientesN :: Busqueda -> Int -> [(String, Int)]   --O(n * (P + log P + log A)) siendo n el numero de productos indicados como argumento.
siguientesN _ 0 = []
siguientesN b n = case siguiente b of
                    (Just par, bR) -> par : (siguientesN bR (n-1))
                    (Nothing, _)  -> []

--EJERCICIO 2 
data Busqueda = B (Map String (Map String Int))
                    [Filtro]

Invariantes:
    *La lista de Filtros no admite repetidos.
    *En el map de nombre producto x atributos, si hay un nombre producto, como minimo debe tener registrado el atributo clave precio, con su valor.
    *Todos los valores de atributos deben ser mayores que cero.

--EJERCICIO 3
registrar :: String -> Int -> Map String Int -> Busqueda -> Busqueda        --O((F * log A) + log P)
registrar nombre precio atributos (B mP filtros) = let atributosN = (assocM "precio" precio atributos) in   --O(log A) siendo A la cantidad de atributos en el map
                                            if (cumpleLosFiltros filtros atributosN)  --O(F * log A)
                                                then case lookupM nombre mP of          --O(log P) siendo P la cantidad de Productos en el map.
                                                        Just _  -> (B mp filtros)
                                                        Nothing -> (B (assocM nombre atributosN mp) filtros) --O(log P) siendo P la cantidad de
                                                else (B mp filtros)                                          -- Productos en el map.

cumpleLosFiltros :: [Filtro] -> Map String Int -> Bool      --O(F * log A) siendo F la cantidad de filtros en la lista y A la cantidad de atributos
cumpleLosFiltros [] _ _ = True                                                                                                      --en el map
cumpleLosFiltros (f:fs) atributos = aplica f atributos && cumpleLosFiltros fs atributos 

--Como el costo de atributosN en registrar (log A) es menor a el de cumpleLosFiltros (F * log A) no se lo escribe para el costo final

filtrar :: Filtro -> Busqueda -> Busqueda
filtrar filtro (B mP filtros) = (B (aplicarFiltros (keysM mp) mP filtro) (filtro:filtros)) --O(P * (log P + log A))

aplicarFiltros :: [String] -> Map String (Map String Int) -> Filtro -> Map String (Map String Int) --O(P * (log P + log A))
aplicarFiltros [] mP filtro = mP
aplicarFiltros (n:ns) mP filtro = case lookupM n mP of                              --O(log P) siendo P las claves del map mP
                                    Just atributos -> if (aplica filtro atributos)  --O(log A)
                                                        then aplicarFiltros ns mP filtro
                                                        else deleteM n (aplicarFiltros ns mP filtro) --O(log P) siendo P los productos del map mP
                                    Nothing        -> error "No existe el producto."

--Como el costo de keysM es O(P) y el de aplicarFiltros es O(P * (log P + log A)) no se escribe para el costo final porque es menor.


------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Parcial History

data Dir = Norte | Este | Sur | Oeste
data Color = Azul | Negro | Rojo | Verde

type Coord = (Int, Int)
type Cell = (Int, Int, Int, Int)
            a     n    r    v
data ChangeType =
    NoChange 
    | ChangeCell Coord
    | HeadChange

data GBBoard =
    GBB Cord
        (History Coord)
        (Map Coord (History Cell))
        (History ChangeType)

1. Como usuario de GBBoard hacer un programa que ponga una bolita de color Rojo en cada celda del tablero.

irAlBorde :: Dir -> GBBoard -> GBBoard
irAlBorde dir gbb = if puedeMover dir gbb
                        then irAlBorde dir (mover dir gbb)
                        else gbb

ponerTodasRojas :: GBBoard -> GBBoard
ponerTodasRojas gbb = ponerFilasRojas(irAlBorde Oeste (irAlBorde Sur gbb))

ponerFilasRojas :: GBBoard -> GBBoard
ponerFilasRojas gbb = if puedeMover Norte gbb
                        then llenarFilaDeRojas(ponerFilasRojas(mover Norte gbb))
                        else llenarFilaDeRojas(gbb)

llenarFilaDeRojas :: GBBoard -> GBBoard
llenarFilaDeRojas gbb = if puedeMover Este gbb
                            then poner Rojo (llenarFilaDeRojas (mover Este gbb))
                            else irAlBorde Oeste (poner Rojo gbb)

EJERCICIO 2
Dar Invariantes para TAD GBBoard e implementar todas sus funciones

INVARIANTES: 
*El par de la posicion del cabezal siempre debe manejarse sobre los limites de la coordenada del tablero.
*Las coordenadas del tablero siempre deben ser mayores a 0, por lo tanto la posicion del cabezal, tambien.
*La cantidad de bolitas que hayan en una celda, nunca puede ser negativa.
*Si ChangeCell c se encuentra en el historial de cambios hC, entonces debe haber una entrada para c en el mapa mC.

emptyGBB :: Int -> Int -> GBBoard
emptyGBB x y = (GBB (x, y) (newH (1,1)) (emptyM) (newH NoChange)) --O(1)



poner :: Color -> GBBoard -> GBBoard                              --O(log C)
poner c (GBB coord h mC hC) = GBB coord h ((registrarCambio c (current h) mC)) (register (ChangeCell (current h)) hC)

registrarCambio :: Color -> Coord -> Map Coord (History Cell) -> Map Coord (History Cell)
registrarCambio col coord mC = case lookupM coord mC of         --O(log C)
                                Just history -> assocM coord (register(agregarColor col (current history)) history) mC --O(log C)
                                Nothing -> assocM coord (newH (agregarColor col (0,0,0,0))) mC                          --O(log C)

agregarColor :: Color -> Cell -> Cell                   --O(1)
agregarColor Azul (a, n, r, v) = ((a + 1), n, r, v)
agregarColor Negro (a, n, r, v) = (a, (n + 1), r, v)
agregarColor Rojo (a, n, r, v) = (a, n, (r + 1), v)
agregarColor Verde (a, n, r, v) = (a, n, r, (v + 1))

--Al haber 3 O(log C) en el registrarCambio, por promedio se debe escribir en el resultado final O(log C).

nroBolitas :: Color -> GBBoard -> Int                           --O(log C) siendo C la cantidad total de celdas en el tablero
nroBolitas color (GBB _ h mC _) = case lookupM (current h) mC of --O(log C)
                                    Just history -> bolitasDeColor color (current history)
                                    Nothing      -> 0

bolitasDeColor :: Color -> Cell -> Int --O(1)
bolitasDeColor Azul (a, n, r, v) = a
bolitasDeColor Negro (a, n, r, v) = n
bolitasDeColor Rojo (a, n, r, v) = r
bolitasDeColor Verde (a, n, r, v) = v



puedeMover :: Dir -> GBBoard -> Bool            --O(1)
puedeMover Norte gbb = (snd (coord gbb)) > (snd (posicionDeCabezal gbb))
puedeMover Este gbb  = (fst (coord gbb)) > (fst (posicionDeCabezal gbb))
puedeMover Sur gbb   = (snd (posicionDeCabezal gbb)) > 1
puedeMover Oeste gbb = (fst (posicionDeCabezal gbb)) > 1

coord :: GBBoard -> (Int, Int)                  --O(1)
coord (GBBoard coord _ _ _) = coord

posicionDeCabezal :: GBBoard -> Coord           --O(1)
posicionDeCabezal (GBBoard _ h _ _) = current h



undo :: GBBoard -> GBBoard                                                  --O(log C)
undo (GBB coord h mC hC) = if esChangeCell (current hC)                     .--O(1)
                            then case lookupM (current h) mC of             --O(log C)
                                    Just history -> (GBB coord (undo h) (assocM (current h) (undo history) mC) (undo hC))  --O(log C)
                                    Nothing      -> error "Tiene que poder revertirse una accion."
                            else (GBB coord (undo h) mC (undo hC))

esChangeCell :: ChangeType -> Bool --O(1)
esChangeCell (ChangeCell _) = True
esChangeCell _ = False



redo :: GBBoard -> GBBoard                                                  --O(log C)
redo (GBB coord h mC hC) =  let hC' = redo hC in
                            if esChangeCell (current hC')                   --O(1)
                            then case lookupM (current h) mC of             --O(log C)
                                    Just history -> (GBB coord h (assocM (current h) (redo history) mC) hC')  --O(log C)
                                    Nothing      -> error "Tiene que poder rehacer una accion."
                            else (GBB coord (redo h) mC hC')



EJERCICIO 3 

data History = H (Stack a) (Stack a)
                  pasado    futuro
INVARIANTES:
*El 1er stack que representa el pasado, nunca puede estar vacio, a menos que se inicialice.
*El 2do stack que representa el futuro puede estar vacio.
*La funcion undo no puede hacerse si el stack pasado tiene un solo elemento.