-- Continuação da aula 9                                           CTRL + C -> INTERROMPE FUNÇÃO NO TERMINAL

-- take n l      constroi uma lista com os n primeiros elementos de l
-- take 3 [1..10] = 1,2,3

ordena4 [] = []
ordena4 [x] = [x]
ordena4 l = juntaListasOrdenadas l1' l2'
    where -- l1 = take (div (length l) 2) l
          -- l2 = drop (div (length l) 2) l
          (l1,l2) = splitAt (div(length l) 2) l
          -- (l1,l2) = parte l
          l1' = ordena4 l1
          l2' = ordena4 l2
          juntaListasOrdenadas :: Ord a => [a] -> [a] -> [a]  -- esta função é conhecida como merge
          juntaListasOrdenadas [] b = b
          juntaListasOrdenadas a [] = a
          juntaListasOrdenadas (a:as) (b:bs) = if (a<b) then a:juntaListasOrdenadas as (b:bs)
                                               else b : juntaListasOrdenadas (a:as) bs
                                

parte :: [a] -> ([a],[a])
parte [] = ([],[])
parte [x] = ([x],[])
parte (x1:x2:xs) = (x1: xs1, x2: xs2)
    where (xs1,xs2) = parte xs

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



{-
elemIndices 'a' "baca"
=         elemIndicesXPTO 0 'a' "baca"
=         elemIndicesXPTO 1 'a' "aca"
=     1 : elemIndicesXPTO 2 'a' "ca"
=     1 : elemIndicesXPTO 3 'a' "a"
= 1 : 3 : elemIndices XPTO 4 'a' ""
= 1 : 3 : []
-}


-- Exercicio 35 da ficha 50qs

menor :: String -> String -> Bool

{- exemplos
    menor "ana" "abel" = False
    menor "abel" "ana" = True
    menor "ana" "anabela" = True
    menor "ana" "anabela" = False -}
    
menor [] []         = False
menor [] (y:ys)     = True
menor (x:xs) []     = False
menor (x:xs) (y:ys)  | x == y = menor xs ys
                     | x > y = False
                     | x < y = True