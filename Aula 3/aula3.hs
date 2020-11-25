{-

    Funções: A -> B é o tipo das funcoes com argumento do tipo A e resultado do tipo B
    Listas ou sequências: [ A ] listas em que todos os elementos são do tipo A 
                        Exemplos de listas : ...  :: [Int]                            * [Int] = [1,2,3] || [1,3,2] || [1,1,1,1,2,2,3]
                                            [[1,2,3]] :: [[Int]] - Esta lista só tem um elemento *Lista dentro de lista*
                                            [´0´,´l´,´a´] :: [Char]
                                            "ola" :: [Char] - Completamente igual ao interior
                                            "123" == [´1´,´2´,´3´]

    Igual: a = b -> a define-se como b
           a == b -> os valores de a e b são o mesmo

-}

maiorC :: Int -> Int -> Int

maiorC x y = if x>y then x else y


comprimento :: [a] -> Int
-- Calcula o numero de elementos de uma lista
-- Definição recursiva
comprimento [] = 0
comprimento l = (comprimento (tail l)) + 1

-- Recursividade

fatorial :: Integer -> Integer
-- 3! = 3*2*1
-- n! = 1 se n = 0
--    = n * (n-1)! se n>0

fatorial n = if n == 0 then 1
            else n * fatorial (n-1)
{-
comprimento [1,2,3]
  = comprimento (tail [1,2,3]) + 1
  = comprimento ([2,3]) + 1
  = comprimento ((tail [2,3]) +1 ) +1
  = comprimento ([3])      +1)  +1
  = comprimento ((tail [3]) +1      +1)  +1
  = comprimento (([])      +1       +1 ) +1
  = comprimento ((tail [])         +1     +1         +1 ) +1
  = (0                + 1           +1) +1
  = 3
-}