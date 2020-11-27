myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (h:t) = (f h) : myMap f t

----------------------------------------------------
-- Capitulo: Já estou cheio de listas
{- 
data Lista a = Vazia | N a (Lista a)

comp :: Lista a -> Int
comp Vazia    = 0
comp (N x xs) = 1 + comp xs
 -}
{- 
1 2 3 representada por :
N 1 (N 2 (N 3 Vazia)) :: Lista Int
 -}
-- Generalizando

-- data ListaXPTO a = Vazia | N a (ListaXPTO a) (ListaXPTO a) -- Árvores
data ABin a = Vazia | N a (ABin a) (ABin a) deriving Show

a1, a2, a3, a4, a5 :: ABin Int

a1 = Vazia
a2 = N 0 Vazia Vazia
a3 = N 5 (N 6 Vazia Vazia) 
         (N 7 Vazia (N 18 Vazia 
                          Vazia))
{- 
                5
              /    \ 
            6       7
                      \
                        18
 -}
a4 = N 5 Vazia 
         (N 4 (N 1 Vazia Vazia) 
         (N 3 Vazia Vazia))   


-- Representação:
{- 
                       10
                   /         \
            5                   18
        /      \               /     \           
    2           7            12        21
              /   \                   /  \
            6      8                19    35
 -}

a5 = N 10 (N 5  (N 2 Vazia Vazia)
                 (N 7 (N 6 Vazia Vazia)
                      (N 8 Vazia Vazia)))
           (N 18 (N 12 Vazia Vazia)
                 (N 21 (N 19 Vazia Vazia)
                       (N 35 Vazia Vazia)))

a6 = N 100 a5 a3

-- Função que diz o comprimento de uma árvore
comp :: ABin a -> Int
comp Vazia = 0
comp (N x y z) = 1 + (comp y) + (comp z)      -- x :: a , y :: ABin a , z :: ABin a

altura :: ABin a -> Int
-- altura a5 == 4
-- altura a3 == 3
altura Vazia = 0
altura (N x y z) = 1 + (max (altura y) (altura z))

mapABin :: (a -> b) -> (ABin a) -> (ABin b)
mapABin f Vazia = Vazia
mapABin f (N r e d) = N (f r) (mapABin f e) (mapABin f d)