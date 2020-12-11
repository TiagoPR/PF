data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

-- Representação:
{- 
                       10
                   /         \
            5                   18
        /      \               /     \           
    2           7            12        21
      \        /   \                   /  \
       4     6      8                19    35
 -}

a5 = Node 10 (Node 5  (Node 2 Empty (Node 4 Empty Empty))
                 (Node 7 (Node 6 Empty Empty)
                      (Node 8 Empty Empty)))
           (Node 18 (Node 12 Empty Empty)
                 (Node 21 (Node 19 Empty Empty)
                       (Node 35 Empty Empty)))

-- a)

altura :: BTree a -> Int
altura Empty = 0
altura (Node n e d) = 1 + (max (altura e) (altura d))

-- b)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node n e d) = 1 + (contaNodos e) + (contaNodos d)

-- c)

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node n Empty Empty) = 1
folhas (Node n e d) = (folhas e) + (folhas d) 

-- d)

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 1 _ = Empty
prune c (Node n e d) = Node n (prune (c-1) e) (prune (c-1) d)

-- h)

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) e d) = (Node a ea da , Node b eb db , Node c ec dc)
    where
        (ea , eb ,ec) = unzipBT e
        (da , db ,dc) = unzipBT d

-- Exercicio 2

-- a) 

minimo :: Ord a => BTree a -> a
minimo (Node n Empty _ ) = n 
minimo (Node n e _) = minimo e 

-- b) 
-- feito por mim
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node n Empty d) = d         -- devolve o que estiver à direita do n(minimo)
semMinimo (Node n e d) = Node n (semMinimo e) d 

-- c) 

minSmin :: Ord a => BTree a -> (a, BTree a)
minSmin (Node n Empty d) = (n,d)
minSmin (Node n e d) = (m , Node n e' d)
    where
        (m,e') = minSmin e

{-
minSmin N 10 (N 8 Empy Empy) Empty = minSmin (N 8 Empty Empty) = (8,Empty)
-}

-- d)

remove ::  Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove n (Node n' e d)
        | n < n' = Node n' (remove n e) d 
        | n > n' = Node n' e (remove n d)
        |otherwise = case d of  -- caso seja igual
            Empty -> e          -- Fica o node à esquerda
            _ -> Node m e d'    -- Vai usar a função minSmin para ir buscar na direita o mais pequeno
        where
            (m,d') = minSmin d 


-- Exercicio 3 

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int | Rep | Faltou deriving Show
type Turma = BTree Aluno -- ´arvore bin´aria de procura (ordenada por n´umero)


-- c)

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (nro, n,r,_) e d) = case r of 
    TE -> trabEst e ++ [(nro,n)] ++ trabEst d -- por ordem , por isso , temos de ter o trabEst e primeiro
    _ -> trabEst e ++ trabEst d               -- ver nos outros ramos da arvore



    