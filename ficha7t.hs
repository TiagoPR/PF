-- Para o Exercicio 4:

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show
-----------------------------------------------------------------------------------------------------------------------------------------

data ExpInt = Const Int | Simetrico ExpInt | Mais ExpInt ExpInt | Menos ExpInt ExpInt | Mult ExpInt ExpInt

-- Exercicio 1 

-- a)

calcula :: ExpInt -> Int 
calcula expr = case expr of
    Const n -> n                -- Se for uma constante só dá ela propria visto que as constantes sõ são folhas
    Simetrico e -> - calcula e
    Mais e1 e2 -> (calcula e1) + (calcula e2) -- e1 e e2 são do tipo ExpInt
    Menos e1 e2 -> (calcula e1) - (calcula e2) 
    Mult e1 e2 -> (calcula e1) * (calcula e2)


-- b)

infixa :: ExpInt -> String
infixa expr = case expr of
    Const n -> show n
    Simetrico e -> "- (" ++ infixa e ++ ")"
    Mais e1 e2 -> "(" ++ (infixa e1) ++ "+" ++ (infixa e2) ++ ")"
    Menos e1 e2 -> "(" ++ (infixa e1) ++ "-" ++ (infixa e2) ++ ")"
    Mult e1 e2 -> "(" ++ (infixa e1) ++ "*" ++ (infixa e2) ++ ")"

-- Exercicio 2

data RTree a = R a [RTree a] deriving Show

-- a)

-- soma (R 12 [R 5 [],R 6 []]) = 23

soma :: Num a => RTree a -> a 
soma (R n l) = sum ([n] ++ map soma l) -- feito p/ mim
    -- n + sum (map soma l)            ->   pela prof.

-- b)

altura :: RTree a -> Int
altura (R n []) = 1
altura (R n l) = 1 + maximum (map altura l)

-- altura (R 12 [R 5 [],R 6 []]) = 1 + maximum (map altura [R 5 [],R 6 []]) = 1 + maximum (altura R 5 [] : (map altura R 6 []))
-- = 1 + maximum [1] = 2

-- d)

mirror ::  RTree a -> RTree a
mirror (R n l) = R n (reverse (map mirror l))

-- mirror (R 12 [R 5 [],R 6 []]) = R 12 (reverse (map mirror [R 5 [],R 6 []])) = 
--    = R 12 (reverse (R 5 (reverse (map mirror []) : map mirror R 6 []))) = 
--    = R 12 (reverse (R 5 (reverse []) : R 6 (reverse []))) =
--    = R 12 (reverse [R 5 [], R 6 []]) = R 12 [R 6 [], R 5 []]

-- e)

-- postorder (R 12 [R 5 [R 12 [],R 13 []],R 6 []]) = [12,13,5,6,12]

postorder :: RTree a -> [a]
postorder (R n l) = (concat (map postorder l)) ++ [n]

-- postorder (R 12 [R 5 [],R 6 []]) = (concat (map postorder [R 5 [],R 6 []])) ++ [12] = 
--    = (concat (concat (map postorder []) ++ [5] : map postorder R 6 []))) ++ [12] = 
--    = (concat ([5] : [6])) ++ [12]) =
--    = (concat ([[5],[6]]) ++ [12]) = [5,6] ++ [12] = [5,6,12]

-- Exercicio 3

data LTree a = Tip a | Fork (LTree a) (LTree a)

f = Fork (Fork (Tip 7) (Tip 10)) (Tip 5)

-- a)

ltSum :: Num a => LTree a -> a
ltSum (Tip n) = n 
ltSum (Fork e d) = ltSum e + ltSum d

-- b)

listaLT :: LTree a -> [a] 
listaLT (Tip n) = [n]
listaLT (Fork e d) = (listaLT e) ++ (listaLT d)

-- Exercicio 4 
 
data FTree a b = Leaf b | No a (FTree a b) (FTree a b) deriving Show

-- a)

-- Nao funciona nem sei o que se passa
{- 
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree a = case a of 
        Leaf b -> (Empty, Tip b)
        No a e d -> (No a x y, Fork x' y')
    where
        (x,x') = splitFTree e
        (y,y') = splitFTree d
 -}
