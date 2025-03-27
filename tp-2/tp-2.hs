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
agregar []  ys  = []
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
factorial n    = n * (factorial (n-1))                    