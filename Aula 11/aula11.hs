import Data.List

-- Aula sobre acumuladores

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x l = elemIndicesXPTO 0 x l


{- elemIndices x (h:t) | x == h = 0: ind
                      | x /= h =    ind
            where ind = elemIndices x t -}

            -- NÃO FUNCIONA!!!

elemIndicesXPTO :: Eq a => Int -> a -> [a] -> [Int]
elemIndicesXPTO p x [] = []
elemIndicesXPTO p x (h:t) | x == h = p : elemIndicesXPTO (p+1) x t
                          | x /= h = elemIndicesXPTO (p+1) x t


soma :: Num a => [a] -> a
soma [] = 0
soma (h:t) = h + (soma t)
{- 
soma [1,2,3]
= 1 + soma [2,3]
= 1 + 2 + soma [3]
= 1 + 2 + 3 + soma []
= 1 + 2 + 3 + 0
= 6
-}

soma1 :: Num a => [a] -> a
soma1 l = somaXPTO 0 l
    where somaXPTO :: Num a => a -> [a] -> a
          somaXPTO acumulador [] = acumulador
          somaXPTO acumulador (h:t) = somaXPTO (acumulador + h) t

{- 
soma1 [1,2,3]
= somaXPTO 0 [1,2,3]
= somaXPTO (0 + 1) [2,3]
= somaXPTO (0 + 1 + 2) [3]
= somaXPTO (0 + 1 + 2 + 3) []
= 6
-}


f [] = []
f (h:t) = h : f t

fXPTO :: [a] -> [a] -> [a]
fXPTO acc [] = acc
fXPTO acc (h:t) = fXPTO (h : acc) t

fastRev l = fXPTO [] l

slowRev [] = []
slowRev l = (last l) : slowRev (init l)

-- Exercicio 41 da ficha 50q

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet l = reorganiza (group l)                       -- group , função definida no Data.List
    where reorganiza :: [[a]] -> [(a,Int)]
          reorganiza [] = []
          reorganiza (h:t) = (head h, length h) : reorganiza t

constroiMSet2 :: Ord a => [a] -> [(a,Int)]
constroiMSet2 [] = []
constroiMSet2 (h:t) = adiciona h (constroiMSet2 t)
    where adiciona x [] = [(x,1)]                           -- É igual ao insereMSet da ficha50q
          adiciona x ((y,n):as) | x == y = (x,n+1) : as
                                | x /= y = (y,n) : adiciona x as

-- constroiMSet [1,1,2,3,3,4,5,6,6,6,7] = [(1,2),(2,1),(3,2),(4,1),(5,1),(6,3),(7,1)]
