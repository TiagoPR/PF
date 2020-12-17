{- 
(+) :: Int -> Int -> Int -- muita restritiva

(+) :: a -> a -> a

'a' :: Char

(+) :: Num a => a -> a -> a
    (tipo da função) a -> a-> a
    (tipo do tipo a) a é uma instância da classe Num

    

Classes em Haskell

-------------------------------------------------------

1. Definir uma classe                                                                           -}

class NOME a where                     -- A classe NOME é constituida pelos tipos a em que ...
--  restrições: funções que têm de existir

class MyEq a where                       -- A classe MyEq é constituida pelos tipos a em que ...
 igual :: a -> a -> Bool
 dif :: a -> a -> Bool

{- 
class Eq a where                       -- A classe Eq é constituida pelos tipos a em que ...
 (==) :: a -> a -> Bool
 (/=) :: a -> a -> Bool                                                                            
 
2. Definir instâncias                                                                            -}

data BTree a = Empty | Node a (BTree a) (BTree a)
    deriving Eq  -- isto faz o mesmo que o instance que fiz (o Eq verifica a igualdade textualmente (ordem,etc))                   

{- 
-- fazer com que BTree a seja uma instância de Eq

instance Eq a => Eq (BTree a) where -- adicionar o Eq a para corrigir o erro abaixo

    -- para fazer isto preciso definir as funções (==) e (/=) com os tipos respetivos 

    -- (==) :: BTree a -> BTree a -> Bool           o ghci nao gosta porque ja sabe o tipo, o mesmo para o diferente
    Empty == Empty = True
    (Node x y z) == (Node p q r) = (x == p)      -- dá erro pq compara cenas do tipo a , por isso temos de por uma restricao no instance
                                   && (y == q)   -- estes nao tem problema pq esta a comparar as BTree
                                   && (z == r)   -- estes nao tem problema pq esta a comparar as BTree
    _==_ = False

    -- (/=) :: BTree a -> BTree a -> Bool
    a1 /= a2 = not (a1 == a2)
.                                                                                                                                 -}
x , y :: BTree Int
x = Node 4 (Node 5 Empty Empty) Empty
y = Node 3 (Node 42 Empty Empty) Empty