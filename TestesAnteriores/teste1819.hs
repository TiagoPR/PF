import Data.Char (isDigit,isAlpha) -- possivel importar simplesmente as funcoes e nao tudo

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

instance Show a => Show (Seq a) where
        show x = "<<" ++ showaux x ++ ">>"

showaux Nil = ""
showaux (Cons a Nil) = show a
showaux (Cons a s) = show a ++ "," ++ showaux s
showaux (App s1 s2) = showaux s1 ++ "," ++ showaux s2