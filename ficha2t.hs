import Data.Char

-- Exercicio 1 no Ipad



-- Exercicio 2

-- a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = h*2 : dobros(t)

-- b)
numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre c s = if (c == head s)
    then 1 + (numOcorre c (tail s))
    else (numOcorre c (tail s))

-- alternativa

numOcorre2 c (h:t) = if (c == h)  -- NOT WORKING
    then 1 + (numOcorre2 c t)
    else (numOcorre2 c t)

-- c)

positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = (h > 0 && (positivos t))

-- d)

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if (h>0) then h : (soPos t)
    else (soPos t)

-- e)

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if (h<0) then h + (somaNeg t)
    else (somaNeg t)

-- f)

tresUlt :: [a] -> [a]
tresUlt l = if (length l <= 3) then l
    else (tresUlt (tail l))

-- g)

segundo :: [(a,b)] -> [b]
segundo [] = []
segundo ((a,b):xs) = b : segundo xs

-- h)

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros x [] = False
nosPrimeiros x ((a,b):xs) = if (x==a) then True
    else nosPrimeiros x xs

-- i)

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)         -- Fazer por escrito para compreender melhor
sumTriplos [] = (0,0,0)
sumTriplos [(a,b,c)] = (a,b,c)
sumTriplos ((a,b,c):(x,y,z):xs) = sumTriplos ((a+x,b+y,c+z):xs)

-- Exercicio 3

-- a)

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (x:xs) = if (isDigit x) then x : (soDigitos xs)
    else (soDigitos xs)

-- b)

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) = if (isLower h) then 1 + (minusculas t)
    else minusculas t

-- c)

nums :: String -> [Int]
nums [] = []
nums (x:xs) = if (isDigit x) then (digitToInt x) : (nums xs)
    else (nums xs)


-- Exercicio 4

type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- a)

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta e ((c,e'):ms) = if (e == e') then 1 + (conta e ms)
    else (conta e ms)

-- b)

grau :: Polinomio -> Int
grau [] = 0
grau [(c,e)] = e
grau ((c,e):(c2,e2):ms) = if (e > e2) then (grau ((c,e):ms))
    else (grau ((c2,e2):ms))

-- c)

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau g ((a,b):ms) = if (g == b) then (a,b) : (selgrau g ms)
    else (selgrau g ms)