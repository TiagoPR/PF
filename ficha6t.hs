data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

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

minimo :: Ord a => BTree a -> a
minimo (Node n Empty _ ) = n 
minimo (Node n e _) = minimo e 