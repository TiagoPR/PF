-- Exercicio 50 da ficha questoes
data Semaforo = Verde | Amarelo | Vermelho deriving Show


interseccaoOk :: [Semaforo] -> Bool
interseccaoOk l = (contaNaoVermelhos l) <= 1
    where contaNaoVermelhos :: [Semaforo] -> Int
          contaNaoVermelhos [] = 0
          contaNaoVermelhos (Vermelho:t) = contaNaoVermelhos t
          contaNaoVermelhos (_:t) = 1 + contaNaoVermelhos t

dobros :: [Int] -> [Int]
-- multiplica todos os elementos por 2
-- dobros [] = []
-- dobros (h:t) = h*2 : dobros t
dobros l = xxxx f l
    where f x  = 2*x

triplos :: [Int] -> [Int]
-- multiplica todos os elementos por 3
-- triplos [] = []
-- triplos (h:t) = h*2 : triplos t
triplos l = xxxx f l
    where f x = 3*x

maisUm :: [Int] -> [Int]
-- soma 1 a todos os elementos
-- maisUm [] = []
-- maisUm (h:t) = (1 + h) : maisUm t
maisUm l = xxxx f l
    where f x = 1 + x

-- todas as funções são basicamente isto
-- xxxx :: (a -> a) -> [a] -> [a]                Esta função chama-se map
xxxx :: (a -> b) -> [a] -> [b]
-- xxxx muda [] = []
-- xxxx muda (h:t) = (muda h) : (xxxx muda t)
xxxx muda l = [ muda x | x <- l]  -- listas por compreensão

-- Funções de ordem superior : Funções que recebem funções como argumento
-- em Haskell
-- map :: (a -> b) -> [a] -> [b]

{- 
Sabendo que (muda h) :: a     h :: a  então muda :: ????

x :: Int         f x :: Float     
    Qual é o tipo de f ?  é uma função que recebe Int e produz Float,i.e., f :: Int -> Float

Logo muda :: a -> a
 -}

{-  Igualdade de funções
  f == g se e só se f x == g x   -- duas funcoes sao iguais quando dão sempre os mesmos valores
 -}



{- max :: a -> (a -> a )
 (max 3) 4 = 4

max :: (a,a) -> a
 max (3,4) = 4 -}


-- data Either a b = Left a | Right b

ex1,ex2,ex3,ex4 :: Either Char Int
ex1 = Left 'a'
ex2 = Right 10
ex3 = Left 'b'
ex4 = Left 'c'

l1 :: [Either Char Int]
l1 = [ex1,ex2,ex3,ex4]

partitionEithers :: [Either a b] -> ([a],[b])
-- partitionEithers l1 = (['a','b','c'],[10])
-- ....
