module StackV1
    (Stack, emptySt, isEmptySt, push, top, pop, lenS)
    where

data Stack a = St [a]
    

emptySt :: Stack a
isEmptySt :: Stack a -> Bool
push :: a -> Stack a -> Stack a
top :: Stack a -> a
pop :: Stack a -> Stack a
lenS :: Stack a -> Int
--Costo: constante

emptySt           = St []
isEmptySt (St xs) = null xs
push a (St xs)   = agregarEn a xs
top (St xs)      = head xs
pop (St xs)      = sinElPrimero xs
lenS (St xs)     = length xs

agregarEn :: a -> [a] -> Stack a
agregarEn a [] = St [a]
agregarEn a xs = St (a:xs)

sinElPrimero :: [a] -> Stack a
sinElPrimero []     = St []
sinElPrimero (_:xs) = St xs