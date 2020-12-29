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

a1 = Node 5 (Node 10 Empty Empty) (Node 2 Empty Empty)

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

-- e)

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node n e d) = [n]
path (h:t) (Node n e d) = case h of 
    True -> n : path t d 
    False -> n : path t e 

-- f)

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node n e d) = Node n (mirror d) (mirror e)

-- g)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node n e d) (Node n1 e1 d1) = Node (f n n1) (zipWithBT f e e1) (zipWithBT f d d1)


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
type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)

turma5a :: Turma
turma5a = Node (10,"Joana",ORD,Aprov 13) (Node (5,"Ana",TE,Aprov 16) (Node (3,"Lara",ORD,Rep) Empty Empty) Empty) 
                                         (Node (15,"Tiago",MEL,Aprov 10) Empty Empty) 
{- 
                              10 Joana
                            /          \
                         5 Ana        15 Tiago 
-}
-- a)

inscNum :: Numero -> Turma -> Bool
inscNum num Empty = False
inscNum num (Node (n,_,_,_) e d)
    | num == n = True
    | otherwise = (inscNum num e) || (inscNum num d)

-- b)

inscNome :: Nome -> Turma -> Bool
inscNome nom Empty = False
inscNome nom (Node (_,n,_,_) e d) = (nom == n) || (inscNome nom e) || (inscNome nom d)
    
-- c)

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (nro, n,r,_) e d) = case r of 
    TE -> trabEst e ++ [(nro,n)] ++ trabEst d -- por ordem , por isso , temos de ter o trabEst e primeiro
    _ -> trabEst e ++ trabEst d               -- ver nos outros ramos da arvore

-- d)

nota :: Numero -> Turma -> Maybe Classificacao
nota num Empty = Nothing
nota num (Node (n,_,_,clas) e d) = if (num == n) then Just clas
    else (if (num < n) then (nota num e) else (nota num d))

-- e)

percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas t = ((faltasaux t) * 100) / totalalumni t
    where
        faltasaux :: Turma -> Float
        faltasaux Empty = 0
        faltasaux (Node (_,_,_,clas) e d) = case clas of
            Faltou -> 1 + faltasaux e + faltasaux d
            _ -> faltasaux e + faltasaux d


totalalumni :: Turma -> Float
totalalumni Empty = 0
totalalumni (Node n e d) = 1 + totalalumni e + totalalumni d

-- f)

mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov t = fromIntegral ((aprovaux t) `div` totalalumnia t)
    where
        aprovaux :: Turma -> Int
        aprovaux Empty = 0
        aprovaux (Node (_,_,_,clas) e d) = case clas of
            Aprov n -> n + aprovaux e + aprovaux d
            _ -> aprovaux e + aprovaux d
        totalalumnia :: Turma -> Int
        totalalumnia Empty = 0
        totalalumnia (Node (_,_,_,clas) e d) = case clas of
            Aprov n -> 1 + totalalumnia e + totalalumnia d
            _ -> totalalumnia e + totalalumnia d

-- g)

-- calcula a quantidade de aprovados e reprovados

aprovAv :: Turma -> Float
aprovAv Empty = 0
aprovAv turma = a / b
    where (a,b) = aux turma
          aux Empty = (0,0)
          aux (Node (_,_,_,clas) e d) = case clas of
              Aprov n -> (x+1,y)
              Rep -> (x,y+1)
              _ -> (x,y)
           where
              (x,y) = (f+h, g+j)   -- aux devolve algo de (.. , ..) logo:
              (f,g) = aux e
              (h,j) = aux d
        
----------------------------------------------------------------------------------------------------------------------------------------------