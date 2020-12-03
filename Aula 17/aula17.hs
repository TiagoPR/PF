
elem' :: Eq a => a -> [a] -> Bool
elem' x  [] = False 
elem' x (h:t) | x == h = True
             | otherwise = elem' x t 

elemO :: Ord a => a -> [a] -> Bool
-- recebe uma lista ordenada crescente
elemO x [] = False
elemO x (h:t)
    | x == h = True
    | x < h = False  -- Porque ao ser uma lista crescente ja sabemos que o x nao vai estar na lista 
    | otherwise = elemO x t 


data ABin a = Vazia | N a (ABin a) (ABin a) deriving Show

a5 = N 10 (N 5  (N 2 Vazia Vazia)
                 (N 7 (N 6 Vazia Vazia)
                      (N 8 Vazia Vazia)))
           (N 18 (N 12 Vazia Vazia)
                 (N 21 (N 19 Vazia Vazia)
                       (N 35 Vazia Vazia)))

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

elemA :: Eq a => a -> ABin a -> Bool
elemA x Vazia = False
elemA x (N r e d) 
    | x == r = True
    | otherwise = (elemA x e) || (elemA x d)  


-- Árvores binárias de procura :
-- Elementos à esquerda são menores ou iguais à raiz
-- Elementos à esquerda são maiores ou iguais à raiz
-- Por isso os elementos à esquerda são todos menores ou iguais que os elementos à direita

procura :: Ord a => a -> ABin a -> Bool
-- recebe uma arvore de procura 
procura x Vazia = False
procura x (N r e d) | x == r = True
                    | x < r = procura x e
                    | x > r = procura x d

acrescenta :: Ord a => a -> ABin a -> ABin a 
-- recebe uma árvore de procura
-- retorna uma árvore de procura
acrescenta x Vazia = N x Vazia Vazia
acrescenta x (N r e d)
    | x <= r = N r (acrescenta x e) d 
    | otherwise = N r e (acrescenta x d)

-----------------------------------------------------------------------------------------------------------------
-- Off topic

fromList :: Ord a => [a] -> ABin a 
fromList [] = Vazia
fromList (h:t) = acrescenta h (fromList t)

-- Parametros      (Função)   (Valor quando chega ao final é Vazia) (Lista à escolha)
fromList' l = foldr acrescenta Vazia l  -- queremos percorrer uma lista logo fazemos um foldr
------------------------------------------------------------------------------------------------------------------

maior :: ABin a -> a 
-- recebe uma arvore de procura não vazia 
maior (N r _ Vazia) = r          -- underscore ou r e 
maior (N _ _ d) = (maior d)
