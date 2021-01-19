import Data.Char (isDigit,isAlpha) -- possivel importar simplesmente as funcoes e nao tudo
import System.Random (randomRIO)
-- Exercicio 1

-- a)

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices _ [] = []
elemIndices n l = indices n 0 l
    where
        indices _ _ [] = []
        indices n acc (h:t) = if (n == h) then acc : indices n (acc + 1) t else indices n (acc + 1) t

-- b)

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf _ [] = False
isSubsequenceOf [] _ = True
isSubsequenceOf (x:xs) (y:ys) = if (x == y) then isSubsequenceOf xs (y:ys) 
        else isSubsequenceOf (x:xs) ys

-- Exercicio 2

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

bt1 = (Node (2,'c') (Node (1,'x') Empty Empty) (Empty))

-- a)

lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP _ Empty = Nothing
lookupAP v (Node (n,m) e d)
        | v == n = Just m
        | otherwise = if (v < n) then lookupAP v e else lookupAP v d

-- b)

bt2 = Node 3 (Node 2 Empty Empty) (Node 4 Empty Empty)
bt3 = Node 5 (Node 3 Empty Empty) (Node 4 Empty Empty)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ Empty Empty = Empty
zipWithBT f (Node n e d) (Node n1 e1 d1) = Node (f n n1) (zipWithBT f e e1) (zipWithBT f d d1)


-- Exercicio 3

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:t)
                | isDigit h = (h:x,y)                       -- é preciso importar as funções
                | isAlpha h = (x,h:y)
        where
                (x,y) = digitAlpha t

-- Exercicio 4

data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a) 

-- a)

firstSeq :: Seq a -> a
firstSeq (Cons n s) = n
firstSeq (App a b) = firstSeq a

-- b)
-- Não sei fazer 

-- c)

{- instance Show a => Show (Seq a) where
        show x = "<<" ++ showaux x ++ ">>"

showaux Nil = ""
showaux (Cons a Nil) = show a
showaux (Cons a s) = show a ++ "," ++ showaux s
showaux (App s1 s2) = showaux s1 ++ "," ++ showaux s2
 -}
------------------------------------------------------------------------------------------

-- re-do

-- Exercicio 1 

-- (a)

elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' n l = elemaux 0 n l
        where
                elemaux _ _ [] = []
                elemaux acc n (h:t)
                        | n == h = acc : elemaux (acc+1) n t
                        | otherwise = elemaux (acc+1) n t


isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' _ [] = False
isSubsequenceOf' [] _ = True
isSubsequenceOf' (x:xs) (y:ys) 
        | x == y = isSubsequenceOf' xs (y:ys)
        | otherwise = isSubsequenceOf' (x:xs) ys

-- Exercicio 2

-- a)

lookupAP' :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP' _ Empty = Nothing
lookupAP' m (Node (a,b) e d)
        | m == a = Just b
        | m < a = lookupAP' m e
        | otherwise = lookupAP' m d

-- b)

zipWithBT' :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT' _ Empty Empty = Empty
zipWithBT' f (Node n e d) (Node n1 e1 d1) = Node (f n n1) (zipWithBT' f e e1) (zipWithBT' f d d1)

-- Exercicio 3

digitAlpha' :: String -> (String,String)
digitAlpha' s = foldl (\(a,d) x -> if isDigit x then (a,x:d) else (x:a,d)) ([],[]) s      -- EASY BRUFF (jk)

-- Exercicio 4


-- a) 

firstSeq' :: Seq a -> a
firstSeq' (Cons a _) = a
firstSeq' (App Nil s) = firstSeq s
firstSeq' (App e d) = firstSeq' e   -- pq à direita esta outro App

-- b)

dropSeq :: Int -> Seq a -> Seq a               -- se c) nao estiver definida usar deriving Show
dropSeq n Nil = Nil
dropSeq n (Cons a s) = dropSeq (n-1) s
dropSeq n (App s s1)
                | (n > nx) = dropSeq (n-nx) s1
                | (n == nx) = s1 
                | otherwise = (App (dropSeq n s) s1)
        where
                nx = contacons s

contacons :: Seq a -> Int
contacons Nil = 0
contacons (Cons a s) = 1 + contacons s
contacons (App s s1) = contacons s + contacons s1


-- c)

instance Show a => Show (Seq a) where
        show x = "<<" ++ mostrar x ++ ">>"

mostrar :: Show a => Seq a -> String
mostrar (Nil) = ""
mostrar (Cons a Nil) = show a
mostrar (Cons a s) = show a ++ "," ++ mostrar s 
mostrar (App s s2) = mostrar s ++ "," ++ mostrar s2 

-- Exercicio 5

type Mat a = [[a]]

-- a)

getElem :: Mat a -> IO a
getElem m = do
        let (linhas,colunas) = (length m, length (head m))
        linha <- randomRIO (0, linhas-1)
        coluna <- randomRIO (0, colunas-1)
        return ((m !! linha) !! coluna)

-- b)
 
magic :: Mat Int -> Bool
magic m = sumlinha n m && sumcoluna n m && sumdiagonais n m
        where n = sum (head m)

sumlinha :: Int -> Mat Int -> Bool 
sumlinha n m = foldl (\acc l -> sum l == n && acc) True m

sumcoluna :: Int -> Mat Int -> Bool 
sumcoluna n m = foldl (\acc x -> sum (map (\l -> l !! x) m) == n && acc) True [0..(length m -1)]

sumdiagonais :: Int -> Mat Int -> Bool 
sumdiagonais n m = sum (map (\n -> (m !! n) !! n) [0..(length m -1)]) == n && sum (map (\n -> (m !! n) !! ((length m -1) - n)) [0..(length m -1)]) == n
 
