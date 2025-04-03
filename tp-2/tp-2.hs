--PRACTICA 2

--EJERCICIO 1
--1. 
sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (x:xs) = x + sumatoria xs

--2.
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--3.
sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (x:xs) = (x + 1) : sucesores xs

--4.
conjuncion :: [Bool] -> Bool
conjuncion []        = True
conjuncion (x:xs) = (x && conjuncion xs)

--5.
disyuncion :: [Bool] -> Bool
disyuncion []     = False
disyuncion (x:xs) = x || disyuncion xs 

--6. 
aplanar :: [[a]] -> [a]
aplanar []       = []
aplanar (x:xs) = x ++ aplanar xs

--7.
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []     = False
pertenece e (x:xs) = (e == x) || pertenece e xs

--8.
apariciones :: Eq a => a -> [a] -> Int
apariciones _ []     = 0
apariciones e (x:xs) = if (e == x)
                        then 1 + apariciones e xs
                        else apariciones e xs

--9.
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ []     = []
losMenoresA n (x:xs) = if (n > x)
                        then x : losMenoresA n xs
                        else losMenoresA n xs

--10.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ []     = []
lasDeLongitudMayorA n (x:xs) = if (longitud x > n)
                                then x : lasDeLongitudMayorA n xs
                                else lasDeLongitudMayorA n xs

--11.
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal xs x = xs ++ [x]

--12.
agregar :: [a] -> [a] -> [a]
agregar []  ys  = ys
agregar (x:xs) ys = x : agregar xs ys

--13.
reversa :: [a] -> [a]
reversa []     = []
reversa (x:xs) = agregarAlFinal(reversa xs) x

--14.
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] _          = []
zipMaximos _ []          = []
zipMaximos (x:xs) (y:ys) = if x>y
                            then x : zipMaximos xs ys
                            else y : zipMaximos xs ys

--15.
elMinimo :: Ord a => [a] -> a
elMinimo [x]     = x
elMinimo (x:xs) = if (x < elMinimo xs)
                    then x
                    else elMinimo xs

--EJERCICIO 2
--1.
factorial :: Int -> Int
factorial 0    = 0
factorial 1    = 1
factorial n    = if ( n <= (-1))
                then error "NO PUEDE HACERSE CON NEGATIVOS"
                else n * (factorial (n-1))

--2.
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if (n >= 1)
                    then n : (cuentaRegresiva (n-1))
                    else []

--3.
repetir :: Int -> a -> [a]
repetir n x = if ( n <= 0 )
                then []
                else x : (repetir (n-1) x)

--4.
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _     = []
losPrimeros _ []     = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

--5.
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs     = xs
sinLosPrimeros _ []     = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs

--EJERCICIO 3
--1.
data Persona = P String Int
    deriving Show

juancho :: Persona
juancho = P "Juan Carlos" 32

orfeo :: Persona
orfeo = P "Orfeo" 100

arsene :: Persona
arsene = P "Arsene" 150


--------------------------------


mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ []     = []
mayoresA n (x:xs) = if (esMayorA n x)
                    then x : mayoresA n xs 
                    else mayoresA n xs

esMayorA :: Int -> Persona -> Bool
esMayorA n (P _ e) = e > n


--------------------------------


promedioEdad :: [Persona] -> Int
--PRECOND: la lista al menos posee una persona.
promedioEdad []     = error "LA LISTA NO PUEDE SER VACIA" 
promedioEdad xs = div (sumatoria (edadesDe xs)) (longitud xs)

edadesDe :: [Persona] -> [Int]
edadesDe []     = []
edadesDe ((P _ e):xs) = e : edadesDe xs


--------------------------------


elMasViejo :: [Persona] -> Persona
--PRECOND: la lista al menos posee una persona.
elMasViejo []     = error "LA LISTA NO PUEDE SER VACIA"
elMasViejo [x]    = x
elMasViejo (x:xs) = if ( edad x >= edad (elMasViejo xs))
                    then x
                    else elMasViejo xs

edad :: Persona -> Int
edad (P _ e) = e    

--2.

data TipoDePokemon = Agua | Fuego | Planta
    deriving Show

data Pokemon = ConsPokemon TipoDePokemon Int
    deriving Show

data Entrenador = ConsEntrenador String [Pokemon]
    deriving Show

entrenadorFidel :: Entrenador
entrenadorFidel = ConsEntrenador "Fidel" [bulbazur, charmander,charmander,charmander,charmander,charmander, squirtle]

entrenadorAsh :: Entrenador
entrenadorAsh = ConsEntrenador "Ash Ketchup" [bulbazur, bulbazur]

charmander :: Pokemon
charmander = ConsPokemon Fuego 100

bulbazur :: Pokemon
bulbazur = ConsPokemon Planta 100

squirtle :: Pokemon
squirtle = ConsPokemon Agua 100

cantPokemon :: Entrenador -> Int
cantPokemon entrenador = longitud (pokemonesDe entrenador)

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (ConsEntrenador _ poks) = poks


--------------------------------


cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tipo (ConsEntrenador _ poks) = cantidadDePokemonesDeTipoEn tipo poks

cantidadDePokemonesDeTipoEn :: TipoDePokemon -> [Pokemon] -> Int
cantidadDePokemonesDeTipoEn _ []        = 0
cantidadDePokemonesDeTipoEn tipo (x:xs) = ( contarSiEsDeTipo tipo x ) + (cantidadDePokemonesDeTipoEn tipo xs)

contarSiEsDeTipo  :: TipoDePokemon -> Pokemon -> Int
contarSiEsDeTipo tipo (ConsPokemon t _) = if (sonElMismoTipo tipo t)
                                then 1
                                else 0 

sonElMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonElMismoTipo Agua Agua = True
sonElMismoTipo Fuego Fuego = True
sonElMismoTipo Planta Planta = True
sonElMismoTipo tipo1 tipo2 = False


--------------------------------


cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ tipo entrenador1 entrenador2 = longitud(pokemonesGanadoresDeTipo_De_A_ tipo entrenador1 entrenador2)

pokemonesGanadoresDeTipo_De_A_ :: TipoDePokemon -> Entrenador -> Entrenador -> [Pokemon]
pokemonesGanadoresDeTipo_De_A_ t (ConsEntrenador _ poks1) (ConsEntrenador _ poks2) = ganadoresDeTipo_De_A_ t poks1 poks2

ganadoresDeTipo_De_A_ :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> [Pokemon]
ganadoresDeTipo_De_A_ _  _ []    = []
ganadoresDeTipo_De_A_ _ [] _     = []
ganadoresDeTipo_De_A_ t (x:xs) ys = if ( esPokemon_DeTipo_GanadorContra_ x t ys )
                                    then x : (ganadoresDeTipo_De_A_ t xs ys) 
                                    else ganadoresDeTipo_De_A_ t xs ys

esPokemon_DeTipo_GanadorContra_ :: Pokemon -> TipoDePokemon -> [Pokemon] -> Bool
esPokemon_DeTipo_GanadorContra_ pok t poks = (sonElMismoTipo (tipo pok) t) && (pokemon_lesGanaATodos_ pok poks)

pokemon_lesGanaATodos_ :: Pokemon -> [Pokemon] -> Bool
pokemon_lesGanaATodos_ _ [] = True
pokemon_lesGanaATodos_ pokP (y:ys) = (superaA pokP y) && (pokemon_lesGanaATodos_ pokP ys) 

tipo :: Pokemon -> TipoDePokemon
tipo (ConsPokemon tipo _) = tipo 

superaA :: Pokemon -> Pokemon -> Bool
superaA (ConsPokemon Agua _) (ConsPokemon Fuego _) = True
superaA (ConsPokemon Fuego _) (ConsPokemon Planta _) = True
superaA (ConsPokemon Planta _) (ConsPokemon Agua _) = True
superaA pok1 pok2 = False


--------------------------------


esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ poks) = perteneceAlTipo_UnoDe_ Agua (tiposDe poks) && 
                                           perteneceAlTipo_UnoDe_ Fuego (tiposDe poks) && 
                                           perteneceAlTipo_UnoDe_ Planta (tiposDe poks)

perteneceAlTipo_UnoDe_ :: TipoDePokemon -> [TipoDePokemon] -> Bool
perteneceAlTipo_UnoDe_ _ []     = False
perteneceAlTipo_UnoDe_ t (x:xs) = sonElMismoTipo t x || perteneceAlTipo_UnoDe_ t xs

tiposDe :: [Pokemon] -> [TipoDePokemon]
tiposDe []     = []
tiposDe (x:xs) = tipo x : tiposDe xs 

--3.

data Seniority = Junior | SemiSenior | Senior
    deriving Show

data Proyecto = ConsProyecto String
    deriving Show

data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving Show

data Empresa = ConsEmpresa [Rol]
    deriving Show

atlus :: Empresa
atlus = (ConsEmpresa [developerCrack, managementCrack, developerTrucho, developerCrack1, developerCrack2, developerCrack3])

developerCrack :: Rol
developerCrack = (Developer Senior persona6)
developerCrack1 :: Rol
developerCrack1 = (Developer Senior persona6)
developerCrack2 :: Rol
developerCrack2 = (Developer Senior persona6)
developerCrack3 :: Rol
developerCrack3 = (Developer Senior persona6)

developerTrucho :: Rol
developerTrucho = (Developer Junior persona4Re)

managementCrack :: Rol
managementCrack = (Management SemiSenior persona4Re)

persona6 :: Proyecto
persona6 = (ConsProyecto "Persona 6")

persona4Re :: Proyecto
persona4Re = (ConsProyecto "Persona 4 Rewind")

zelda :: Proyecto
zelda = (ConsProyecto "Zelda")


--------------------------------


proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa roles) = listaDeProyectosSinRepetirDe roles

listaDeProyectosSinRepetirDe :: [Rol] -> [Proyecto]
listaDeProyectosSinRepetirDe []     = []
listaDeProyectosSinRepetirDe (x:xs) = if pertenece (nombreDelProyecto(proyecto x)) (nombresDe (listaDeProyectosSinRepetirDe xs))
                                    then listaDeProyectosSinRepetirDe xs 
                                    else (proyecto x) : (listaDeProyectosSinRepetirDe xs)

nombresDe :: [Proyecto] -> [String]
nombresDe []     = []
nombresDe (x:xs) = nombreDelProyecto x : nombresDe xs

proyectosDe :: [Rol] -> [Proyecto]
proyectosDe []     = []
proyectosDe (x:xs) = (proyecto x) : (proyectosDe xs)

proyecto :: Rol -> Proyecto
proyecto (Developer _ proyecto) = proyecto
proyecto (Management _ proyecto) = proyecto

nombreDelProyecto :: Proyecto -> String
nombreDelProyecto (ConsProyecto nombre) = nombre  


--------------------------------


losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior _ []                   = 0
losDevSenior (ConsEmpresa roles) ps = cantidadDeDevSeniorEnProyectos roles ps 

cantidadDeDevSeniorEnProyectos :: [Rol] -> [Proyecto] -> Int
cantidadDeDevSeniorEnProyectos [] _      = 0
cantidadDeDevSeniorEnProyectos (r:rs) ps = if esDevSenior r 
                                        then (contarDevSeniorSiParticipaEn r ps) + (cantidadDeDevSeniorEnProyectos rs ps)
                                        else cantidadDeDevSeniorEnProyectos rs ps

esDevSenior :: Rol -> Bool
esDevSenior (Developer Senior _) = True
esDevSenior _                    = False

contarDevSeniorSiParticipaEn :: Rol -> [Proyecto] -> Int
contarDevSeniorSiParticipaEn _ []     = 0
contarDevSeniorSiParticipaEn r ys = if pertenece (nombreDelProyecto(proyecto r)) (nombresDe ys)
                                    then 1
                                    else 0

--------------------------------


cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn [] _                       = 0
cantQueTrabajanEn (x:xs) empr = (cantidadQueTrabajanEn (rolesDe(empr)) x) + (cantQueTrabajanEn xs empr)

rolesDe :: Empresa -> [Rol]
rolesDe (ConsEmpresa roles) = roles

cantidadQueTrabajanEn :: [Rol] -> Proyecto -> Int
cantidadQueTrabajanEn [] _     = 0
cantidadQueTrabajanEn (y:ys) p = (contarSiTrabajaEn y p) + (cantidadQueTrabajanEn ys p)

contarSiTrabajaEn :: Rol -> Proyecto -> Int
contarSiTrabajaEn r p = if sonElMismoProyecto (proyecto r) p
                        then 1
                        else 0

--------------------------------

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto empr = asignadosPorProyecto' (rolesDe empr)

asignadosPorProyecto' :: [Rol] -> [(Proyecto, Int)]
asignadosPorProyecto' []     = []
asignadosPorProyecto' (x:xs) = sumarProyectoA (proyecto x) (asignadosPorProyecto' xs)

sumarProyectoA :: Proyecto -> [(Proyecto, Int)] -> [(Proyecto, Int)]
sumarProyectoA p []     = (p , 1) : []
sumarProyectoA p (x:xs) = if sonElMismoProyecto p (fst x)
                            then (fst x,(snd x + 1)) : xs
                            else x : sumarProyectoA p xs

sonElMismoProyecto :: Proyecto -> Proyecto -> Bool
sonElMismoProyecto (ConsProyecto n1) (ConsProyecto n2) = n1 == n2