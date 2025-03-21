--ejercicio 2

--1.a.
sucesor :: Int -> Int
sucesor n = n+1

--b.
sumar :: Int -> Int -> Int
sumar n m = n+m

--c.
--Solucion Original
divisionYResto :: Int -> Int -> (Int, Int)
--PRECOND: m debe distinto a 0.
divisionYResto n m = (div n m, mod n m)

--Solucion Extra
divisionYResto' :: Int -> Int -> (Int, Int)
--PRECOND: m debe distinto a 0.
divisionYResto' n m = if (m/=0)
                        then (div n m, mod n m)
                        else error "NO SE PUEDE DIVIDIR POR 0"

--d.
maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = if (n>m)
                    then n
                    else m

--2.
{-Ejemplo 1.
sumar (maxDelPar( divisionYResto 2 1 )) (sucesor (7))

Ejemplo 2.
sumar (maxDelPar( divisionYResto 100 10 )) (sucesor (-1))

Ejemplo 3.
sumar (maxDelPar (divisionYResto (sucesor 4) 1)) 5

Ejemplo 4.
maxDelPar (divisionYResto (maxDelPar (74, ( sumar( sucesor 12) 137))) 15)
-}

--Ejercicio 3.
data Dir = Norte | Sur | Este | Oeste
    deriving Show

--1.a.
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Este = Oeste
opuesto Sur = Norte
opuesto Oeste = Este

--b.
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Este Este = True
iguales Sur Sur = True
iguales Oeste Oeste = True
iguales _ _ = False

--c.
siguiente :: Dir -> Dir
--PRECOND: no existe la siguiente dirección a Oeste.
siguiente Norte = Este
siguiente Este  = Sur
siguiente Sur   = Oeste
siguiente Oeste = error "NO EXISTE LA SIGUIENTE DIRECCION A OESTE"

{-Necesita una precondicion porque la funcion puede provocar que se rompa en la ejecucion, por lo que tambien es una funcion parcial ya que no 
cubre uno de los casos de los Constructores de Dir.-}

--2.
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving Show

--a.
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

primerDia :: DiaDeSemana
primerDia = Lunes

ultimoDia :: DiaDeSemana
ultimoDia = Domingo

--b.
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes    = True
empiezaConM Miercoles = True
empiezaConM dia       = False

--c.
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues dia1 dia2 = numeracionDelDia dia1 > numeracionDelDia dia2

numeracionDelDia :: DiaDeSemana -> Int
numeracionDelDia Lunes     = 1
numeracionDelDia Martes    = 2
numeracionDelDia Miercoles = 3
numeracionDelDia Jueves    = 4
numeracionDelDia Viernes   = 5
numeracionDelDia Sabado    = 6
numeracionDelDia Domingo   = 7

--d.
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio d1 = (1< numeracionDelDia d1 && numeracionDelDia d1 <7)

--3.a.
negar :: Bool -> Bool
negar True = False
negar False = True

--b.
implica :: Bool -> Bool -> Bool
implica False bool2 = True
implica True bool2  = bool2

--c.
yTambien :: Bool -> Bool -> Bool
yTambien False bool2 = False
yTambien True bool2 = bool2

--d.
oBien :: Bool -> Bool -> Bool
oBien True bool2  = True
oBien False bool2 = bool2

--Ejercicio 4

--1.
data Persona = P String Int
            --  Nombre  Edad
            deriving Show

joker :: Persona
joker = P "Ren Amamiya" 16

messi :: Persona
messi = P "Lionel Messi" 37

--a.
nombre :: Persona -> String
nombre (P n e) = n

--b.
edad :: Persona -> Int
edad (P n e) = e 

--c.
crecer :: Persona -> Persona
crecer (P n e) = (P n (e+1))

--d.
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nombre (P n e) = (P nombre e)

--e.
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P _ e1) (P _ e2) = e1 > e2
esMayorQueLaOtra (P _ e1) (P _ e2) = e1 <= e2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor per1 per2 = if (esMayorQueLaOtra per1 per2)
                            then per1
                            else per2

--2.
data Pokemon = Pok TipoDePokemon Int
        deriving Show

data TipoDePokemon = Agua | Fuego | Planta
        deriving Show

data Entrenador = E String Pokemon Pokemon
        deriving Show

charmander :: Pokemon
charmander = Pok Fuego 100

bulbazur :: Pokemon
bulbazur = Pok Planta 94

squirtle :: Pokemon
squirtle = Pok Agua 90

entrenadorAsh :: Entrenador
entrenadorAsh = E "Ash Ketchup" bulbazur squirtle

entrenadorFidel :: Entrenador
entrenadorFidel = E "Fidel" palkia reshiram

palkia :: Pokemon
palkia = Pok Agua 100

reshiram :: Pokemon
reshiram = Pok Fuego 100

--a.
superaA :: Pokemon -> Pokemon -> Bool
superaA (Pok Agua _) (Pok Fuego _) = True
superaA (Pok Fuego _) (Pok Planta _) = True
superaA (Pok Planta _) (Pok Agua _) = True
superaA pok1 pok2 = False

--b.
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tipo (E _ pok1 pok2) = (contarSiEsDeTipo tipo pok1) + (contarSiEsDeTipo tipo pok2)

contarSiEsDeTipo  :: TipoDePokemon -> Pokemon -> Int
contarSiEsDeTipo tipo1 (Pok t _) = if (sonElMismoTipo tipo1 t)
                                then 1
                                else 0 

sonElMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonElMismoTipo Agua Agua = True
sonElMismoTipo Fuego Fuego = True
sonElMismoTipo Planta Planta = True
sonElMismoTipo tipo1 tipo2 = False

--c.
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon ((E _ pok1 pok2), (E _ pok3 pok4)) = pok1 : pok2 : pok3 : pok4 : []

--Ejercicio 5

--1.a.
loMismo :: a -> a
loMismo a = a

--b.
siempreSiete :: a -> Int
siempreSiete a = 7

--c.
swap :: (a,b) -> (b, a)
swap (a, b) = (b, a)

--2. Estos son funciones polimorficas porque a la misma no le importa los tipos de parametros que recibe, funcionan para todos.

--Ejercicio 6

--2.
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

--3.
elPrimero :: [a] -> a
--PRECOND: La lista debe de tener al menos un elemento.
elPrimero (a:_) = a 

--4.
sinElPrimero :: [a] -> [a]
--PRECOND: La lista debe de tener al menos un elemento.
sinElPrimero (_:xs) = xs

--5.
splitHead :: [a] -> (a, [a])
--PRECOND: La lista debe de tener al menos un elemento.
splitHead (a:xs) = (elPrimero (a:xs), sinElPrimero (a:xs))