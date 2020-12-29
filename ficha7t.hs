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

-- infixa (Mais (Const 3) (Menos (Const 2) (Const 5)))

infixa :: ExpInt -> String
infixa expr = case expr of
    Const n -> show n
    Simetrico e -> "- (" ++ infixa e ++ ")"
    Mais e1 e2 -> "(" ++ (infixa e1) ++ "+" ++ (infixa e2) ++ ")"
    Menos e1 e2 -> "(" ++ (infixa e1) ++ "-" ++ (infixa e2) ++ ")"
    Mult e1 e2 -> "(" ++ (infixa e1) ++ "*" ++ (infixa e2) ++ ")"

-- c)

posfixa :: ExpInt -> String
posfixa expr = case expr of
    Const n -> show n
    Simetrico e -> posfixa e ++ " - "
    Mais e1 e2 -> posfixa e1 ++ posfixa e2 ++ " + "
    Menos e1 e2 -> posfixa e1 ++ posfixa e2 ++ " - "
    Mult e1 e2 -> posfixa e1 ++ posfixa e2 ++ " * "


-- Exercicio 2

data RTree a = R a [RTree a] deriving Show

roset = R 10 [R 5 [R 12 [] , R 13 []] ,
              R 4 [] , 
              R 2 [] , 
              R 1 []]

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

-- c)

prune :: Int -> RTree a -> RTree a
prune 0 (R n l) = R n []
prune i (R n l) = R n (map (prune (i-1)) l)

-- R 10 (map (prune 0) l) = R 10 [(prune 0 [R 5 [R 12 [] , R 13 []]] : (map prune 0 [R 4 [] , R 2 [] , R 1 []]))]
-- = R 10 [R 5 [],R 4 [] , R 2 [] , R 1 []]

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

data LTree a = Tip a | Fork (LTree a) (LTree a) deriving Show

f = Fork (Fork (Tip 7) (Tip 10)) (Tip 5)

-- a)

ltSum :: Num a => LTree a -> a
ltSum (Tip n) = n 
ltSum (Fork e d) = ltSum e + ltSum d

-- b)

listaLT :: LTree a -> [a] 
listaLT (Tip n) = [n]
listaLT (Fork e d) = (listaLT e) ++ (listaLT d)

-- c)

ltHeight :: LTree a -> Int
ltHeight (Tip n) = 1
ltHeight (Fork e d) = (ltHeight e) + (ltHeight d)

-- Exercicio 4 
 
data FTree a b = Leaf b | No a (FTree a b) (FTree a b) deriving Show

fulltree = No 'b' (No 'c' (Leaf 1) (Leaf 2)) (Leaf 4)

-- a)

-- Estava com erro pq o where nao estava com identação correta (aka, esquecime de o por mais para dentro pq os parametros 'e' e 'd' so
--                                          estão instanciados no case of)

splitFTree2 :: FTree a b -> (BTree a, LTree b)
splitFTree2 a = case a of 
        (Leaf b) -> (Empty, Tip b)
        (No a e d) -> (Node a x y, Fork x' y')
            where
                (x,x') = splitFTree2 e
                (y,y') = splitFTree2 d

-- ou

splitFTree :: FTree a b -> (BTree a,LTree b)
splitFTree (Leaf b) = (Empty,Tip b)
splitFTree (No a c d) = (Node a (fst(splitFTree c)) (fst(splitFTree d)) , Fork (snd(splitFTree c)) (snd(splitFTree d)))
