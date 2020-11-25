-- Exercicio 1

numerar :: Int -> Int -> [Int] 
numerar x y = [x ..y]

-- Exercicio 2

numerar2 :: Int -> Int -> Int -> [Int]
numerar2 x y z = [x,y ..z]

-- Exercicio 3 
juntarlistas :: [a] -> [a] -> [a]
juntarlistas t u = t ++ u

-- Exercicio 4
-- o 1º elemento é o 0 ou seja o 2º é o 1 e por isso o (x-1)
elementonumero :: [a] -> Int -> a
elementonumero (h:t) 0 = h
elementonumero (h:t) x = elementonumero t (x-2)

-- Exercicio 5
-- ir sempre trocando a cabeça para ultimo 
reverter :: [a] -> [a]
reverter [] = []
reverter (h:t) = reverter t ++ [h]

-- Exercicio 6

primeiros :: Int -> [a] -> [a]
primeiros _ [] = []
primeiros 1 [h] = [h]
primeiros x (h:t) = h : (take (x-1) t)

-- Exercicio 7

tirarprimeiros :: Int -> [a] -> [a]
tirarprimeiros n [] = []
tirarprimeiros 0 (h:t) = (h:t)
tirarprimeiros n (h:t) = (tirarprimeiros (n-1) t)

-- Exercicio 8

juntarpares :: [a] -> [b] -> [(a,b)]
juntarpares _ [] = []
juntarpares [] _ = []
juntarpares (x:xs) (h:t) = (x,h) : (juntarpares xs t)

-- Exercicio 9 
verificarel :: Eq a =>a-> [a]->Bool
verificarel x [] = False 
verificarel x (h:t) = if x==h then True else ( verificarel x t)

-- Exercicio 10
replicar :: Int -> a -> [a]
replicar 0 _ = []
replicar 1 x = [x]
replicar n x = x : (replicar (n-1) x)

-- Exercicio 11

intercalar :: a -> [a] -> [a]
ĩntercalar x [] = []
intercalar x [h] = [h]
intercalar x (h:t) = h : x : intercalar x t

-- Exercicio 12

agrupar :: Eq a => [a] -> [[a]] 
agrupar [] = []
agrupar (h:t) = takewhile ==h : agrupar (dropwhile ==h) (h:t))
-- output do takewhile é ser igual a h , o dropwhile apenas deixa ficar os que não satisfazem a condição ou seja apenas i




