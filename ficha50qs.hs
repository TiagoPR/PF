-- Exercicio 1

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' ini fin
    |ini > fin = []
    |otherwise = ini : ( enumFromTo' (ini + 1) fin )

-- Exercicio 2

enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' ini step fin
    |ini > step || ini > fin = [ini]
    |ini < step && step < fin = ini : (enumFromThenTo' step (2*step-ini) fin)
    |ini < step && step == fin = [ini,fin]

-- Exercicio 3

junta :: [a] -> [a] -> [a]
junta [] l2 = l2
junta (x:xs) l2 = x : (junta xs l2)

-- Exercicio 4

index :: [a] -> Int -> a
index (h:t) a = 
    if (a == 0) then h
    else index t (a-1)

-- Exercicio 5

reverse' :: [a] -> [a]
reverse' [] = []
reverse' l = last l : (reverse' (init l))

-- Exercicio 6

take' :: Int -> [a] -> [a]                 ---- Fazer de novo o Take and Drop, muito lambão
take' 0 l = l
take' n (h:t) = h : take' (n-1) t
-- incorreto:
--    if (n==1) then [h]  
--    else take' (n-1) t

-- Exercicio 7

drop' :: Int -> [a] -> [a]                     ---- Fazer de novo o Take and Drop, muito lambão
drop' 0 l = l
drop' n [] = []
drop' n (h:t) = drop' (n-1) t 

-- Exercicio 8

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (h:t) (x:xs) = (h,x) : zip' t xs

-- Exercício 9 

elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x [n] = False 
elem' n (h:t) = if (n==h) then True
    else elem' n t

-- Exercicio 10

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' q v = if (q>0) then v : replicate' (q-1) v
    else replicate' 0 v

-- ou uma maneira mais organizada

replicate2 :: Int -> a -> [a]
replicate2 0 _ = []
replicate2 q v 
    |(q>0) = v : replicate2 (q-1) v

-- Exercicio 11

intersperce' :: a -> [a] -> [a]
intersperce' _ [] = []
intersperce' _ [x] = [x]
intersperce' n (h:t) = h : n : intersperce' n t

-- Exercicio 12

group' :: Eq a => [a] -> [[a]]                    -- Funciona
group' [] = []
group' (x:xs) = aux [x] xs
    where
        aux l [] = [l]
        aux (x:xs) (y:ys)
            | x == y = aux (y:x:xs) ys
            | x /= y = (x:xs) : aux [y] ys

{- group' (h:t) = aux  [h] t
    where aux (y:ys) (x:xs) 
                  | y==x =
                  | y/=x = 
          aux l [] = [l]  -}

group2 [] = []                           
group2 [x] = [[x]]                          -- Não funciona
group2 (x:xs) = [x] : (y:ys) :ls
    where ((y:ys):ls) = group2 xs


-- Exercicio 13

concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t -- (junta h (concat' t))

-- Exercicio 14

inits :: [a] -> [[a]]
inits [] = [[]]
inits l = inits (init l) ++ [l]

-- Exercicio 15

tails :: [a] -> [[a]]
tails [] = [[]]
tails (h:t) = (h:t) : (tails t)

-- Exercicio 16

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (h:t) (x:xs) = if (h == x) then isPrefixOf t xs
    else False

-- Exercicio 17

isSuffixOf :: Eq a => [a] -> [a] -> Bool          -- Será que não tem melhor maneira?
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf l1 l2 = isPrefixOf (reverse' l1) (reverse' l2)

-- Exercicio 18

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (h:t) (x:xs) = if (h==x) then isSubsequenceOf t xs
    else isSubsequenceOf (h:t) xs

-- Exercicio 19

elemIndices :: Eq a => a -> [a] -> [Int]        
elemIndices _ [] = []
elemIndices n l = elemIndicesaux 0 n l

elemIndicesaux :: Eq a => Int -> a -> [a] -> [Int]            -- Acumulador FTW!
elemIndicesaux acc a [] = []
elemIndicesaux acc a (h:t)
    | a == h = acc : (elemIndicesaux (acc + 1) a t)
    | a /= h = (elemIndicesaux (acc + 1) a t)

-- Exercicio 20

nub :: Eq a => [a] -> [a]           -- O meu elem' nao funciona pq?
nub [] = []
nub l = nub' l []
    where
        nub' [] _ = []
        nub' (x:xs) ls
            | elem x ls = nub' xs ls
            | otherwise = x : nub' xs (x:ls)


-- Exercicio 21

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete n (h:t) = if (n == h) then t 
    else h : delete n t

-- Exercicio 22 

-- (\\)[1,2,3,4,5,1] [1,5] = [2,3,4,1]

difLista :: Eq a => [a] -> [a] -> [a]      -- FUNCIONA!!! 5*
difLista [] l2 = []
difLista l1 [] = l1
difLista (x:xs) l2
    | elem x l2 = difLista xs (delete x l2)
    | otherwise = x : difLista xs l2

-- Exercicio 23

-- union [1,1,2,3,4] [1,5] = [1,1,2,3,4,5]

union :: Eq a => [a] -> [a] -> [a]
union [] l2 = l2
union l1 [] = l1
union (h:t) (x:xs)
    | h == x = h : (union t xs)
    | otherwise = h : union t (x:xs)

-- Exercicio 24

-- intersect [1,1,2,3,4] [1,3,5] = [1,1,3]

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] l2 = []
intersect l1 [] = []
intersect (x:xs) l2
    | x `elem` l2 = x : intersect xs l2
    | otherwise = intersect xs l2  

{- iguais :: Eq a => [a] -> [a] -> [a]      -- Não funciona, mas o que fiz antes sim
iguais [] _ = []
iguais (x:xs) (y:ys) = if (x == y) then x : iguais xs (y:ys)
    else iguais xs ys -} 

-- Exercicio 25

-- insert 25 [1,20,30,40] = [1,20,25,30,40]

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h:t)
    | x > h = h : insert x t
    | otherwise = x : (h:t)

-- Exercicio 26

-- unwords ["Programacao", "Funcional"] = "Programacao Funcional"          Voltar a fazer juntamente com o 27

unwords' :: [String] -> String
unwords' [] = ""
unwords' [x] = x
unwords' (h:t) = h ++ ' ' : unwords' t


-- "Programacao" = ['P','r','o','g','r','a','m','a','c','a','o']
-- ['P','r','o','g','r','a','m','a','c','a','o',' ','F','u','n'] = "Programacao Fun"

-- Exercicio 27

unlines' :: [String] -> String
unlines' [] = []
unlines' (h:t) = h ++ '\n' : unlines' t 

-- Exercicio 28 

-- pMaior [11,21,31,21] = 2

pMaior :: Ord a => [a] -> Int
pMaior l = pMaiorAux 0 l
    where 
        pMaiorAux :: Ord a => Int -> [a] -> Int
        pMaiorAux n (n1:n2:ns)
            | (n1 < n2) = pMaiorAux (n + 1) (n2:ns)
            | otherwise = n

-- Exercicio 29

{-  temRepetidos [11,21,31,21] = True
    temRepetidos [11,2,31,4] = False -}

temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (h:t) = if ( h `elem` t) then True
    else temRepetidos t

-- Exercicio 30

-- algarismos "123xp5" = "1235"

algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t)
    | (h >= '0') && (h <= '9') = h : algarismos t
--  | (h >= '0') = h : algarismos t                   Esta linha não funciona , perguntar à prof
    | otherwise = algarismos t

-- Exercicio 31

-- posImpares [10,11,7,5] = [11,5]                             FUNCIONA!! *ACUMULADOR FTW*

posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares l = posImparesAux 0 l
    where
        posImparesAux :: Int -> [a] -> [a]
        posImparesAux n [] = []
        posImparesAux n (h:t) = if (n `mod` 2 /= 0) then h : posImparesAux (n+1) t
                    else posImparesAux (n+1) t

-- Exercicio 32

-- posPares [10,11,7,5] = [10,7].

posPares :: [a] -> [a]
posPares [] = []
posPares l = posParesAux 0 l 
    where
        posParesAux :: Int -> [a] -> [a]
        posParesAux n [] = []
        posParesAux n (h:t) = if (n `mod` 2 == 0) then h : posParesAux (n+1) t
            else posParesAux (n+1) t


-- Exercicio 33

-- isSorted [1,2,2,3,4,5] = True, enquanto que isSorted [1,2,4,3,4,5] = False

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted (h:h2:t) = if ( h <= h2) then isSorted (h2:t) else False

-- Exercicio 34

iSort :: Ord a => [a] -> [a]           -- Ver melhor este pelo menos
iSort [] = []
iSort (h:t) = insert' h t' 
    where
        t' = iSort t

insert' :: Ord a => a -> [a] -> [a]
insert' n [] = [n]
insert' n (h:t) = if (n <= h) then n : (h:t) 
    else h : insert n t 

-- Exercicio 35

--  menor "sai" "saiu" = True enquanto que, menor "programacao" "funcional" = False.

menor :: String -> String -> Bool 
menor [] [] = False
menor [] s = True 
menor (x:xs) (y:ys) 
    | (x == y) = menor xs ys
    | (x < y) = True 
    | (x > y) = False

-- Exercicio 36

--  elemMSet ’a’ [(’b’,2), (’a’,4), (’c’,1)] = True enquanto que elemMSet ’d’ [(’b’,2), (’a’,4), (’c’,1)] = False.

elemMSet' :: Eq a => a -> [(a,Int)] -> Bool
elemMSet' _ [] = False
elemMSet' l ((x,x2):xs) 
    | l == x = True
    | otherwise = elemMSet' l xs 

-- Exercicio 37 

-- lengthMSet [(’b’,2), (’a’,4), (’c’,1)] = 7

lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((x,y):xy) = y + lengthMSet xy


-- Exercicio 38

-- converteMSet [(’b’,2), (’a’,4), (’c’,1)] =  "bbaaaac"

-- Minha versão , mais complicada..

converteMSet :: [(a,Int)] -> [a]                                     -- Voltar a fazer
converteMSet [] = []
converteMSet (x:xs) = cMSaux x ++ converteMSet xs

cMSaux :: (a,Int) -> [a]
cMSaux (x,0) = []
cMSaux (x,y) = x : cMSaux (x,y-1)

-- Versão de alguem aí

{- converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,1):xs) = x : converteMSet xs
converteMSet ((x,y):xs) = [x] ++ converteMSet ((x,y-1):xs) -}


-- Exercicio 39

-- insereMSet 'c' [('b',2), ('a',4), ('c',1)] = [('b',2),('a',4),('c',2)]

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet _ [] = []
insereMSet l ((x,y):xs) 
    | l == x = ((x,y+1):xs)
    | otherwise = (x,y) : insereMSet l xs


-- Exercicio 40

-- removeMSet 'c' [('b',2), ('a',4), ('c',1)] = [('b',2),('a',4)]

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet l ((x,y):xs)
    | l == x = xs
    | otherwise = (x,y) : removeMSet l xs


-- Exercicio 41

-- constroiMSet "aaabccc" = [('a',3),('b',1),('c',3)]

constroiMSet :: Ord a => [a] -> [(a,Int)]                   -- Voltar a fazer
constroiMSet [] = []
constroiMSet (x:y:xs) = constroiAux 1 (x:y:xs)

constroiAux n [x] = [(x,n)]
constroiAux n (x:y:xs)
    | (x == y) = constroiAux (n+1) (y:xs)
    | otherwise = (x,n) : constroiAux 1 (y:xs)


-- Exercicio 42

partitionEithers :: [Either a b] -> ([a],[b])          -- Voltar a fazer , Either -> Left a ou Right b
partitionEithers [] = ([],[])
partitionEithers (h:t) = case h of
                            Left a -> ((a:x),y)
                            Right b -> (x,(b:y))
    where
        (x,y) = partitionEithers t

-- Exercicio 43 

catMaybes' :: [Maybe a] -> [a]                     -- Parecido ao Eithers mas é Maybe - tem os valores Just a ou Nothing
catMaybes' [] = []
catMaybes' (h:t) = case h of
    Just a -> a : catMaybes' t
    Nothing -> catMaybes' t


-- Exercicio 44

data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte:xs) = posicao (x,y+1) xs
posicao (x,y) (Sul:xs) = posicao (x,y-1) xs
posicao (x,y) (Este:xs) = posicao (x+1,y) xs
posicao (x,y) (Oeste:xs) = posicao (x-1,y) xs

-- Exercicio 45

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (a,b) (x,y)
    | ((a == x) && (b == y)) = []
    | a<x = Este : caminho (a,b) (x-1,y)
    | a>x = Oeste : caminho (a,b) (x+1,y)
    | b<y = Norte : caminho (a,b) (x,y-1)
    | otherwise = Sul : caminho (a,b) (x,y+1)


-- Exercicio 46

vertical :: [Movimento] -> Bool
vertical [] = True
vertical (h:t) = case h of
    Norte -> vertical t
    Sul -> vertical t 
    _ -> False

-- Exercicio 47

data Posicao = Pos Int Int deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [x] = x
maisCentral ((Pos a b):(Pos x y):t)
    | (dist (Pos a b) > dist (Pos x y)) = maisCentral ((Pos x y):t)
    | otherwise = maisCentral ((Pos a b):t)

dist:: Posicao -> Float
dist (Pos x y) = sqrt (fromIntegral ((x^2)+(y^2)))

-- Exercicio 48

vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos x [] = []
vizinhos (Pos a b) ((Pos x y):t)
    | (a == x-1) || (a == x+1) || (a == y+1) || (a == y-1) = (Pos x y) : vizinhos (Pos a b) t
    | otherwise = vizinhos (Pos a b) t

-- Exercicio 49

mesmaOrdenada :: [Posicao] -> Bool 
mesmaOrdenada [] = True
mesmaOrdenada [x] = True
mesmaOrdenada ((Pos a b):(Pos c d):t)
    | (b == d) = mesmaOrdenada ((Pos c d):t)
    | otherwise = False

-- Exercicio 50

data Semaforo = Verde | Amarelo | Vermelho deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK l = (contaNaoVermelhos l) <= 1
    where
        contaNaoVermelhos :: [Semaforo] -> Int
        contaNaoVermelhos [] = 0
        contaNaoVermelhos (h:t) = case h of
            Vermelho -> contaNaoVermelhos t
            Verde -> 1 + contaNaoVermelhos t 
            Amarelo -> 1 + contaNaoVermelhos t


-- FEITO!!!  Agora só falta voltar a fazer 500* ;-;