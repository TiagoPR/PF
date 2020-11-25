{-
ordena [] = []
ordena (h:t) = ???
    where men = lista dos elementos de t que são menores ou iguais (<=) a h
        mai = lista dos elementos de t que são maiores (>) que h
 -}

-- Definir uma função que ordena uma lista , em Haskell chama-se quickSort
ordena3 [] = []
ordena3 (h:t) = (ordena3 men) ++ (h : (ordena3 mai))
    where (men,mai) = menMai h t
          
          {- men = [x | x <- t, x <= h]                            --- ou menores h t
          mai = [x | x <- t, x >  h] -}                            --- ou maiores h t
          {- menores :: Ord a => a -> [a] -> [a]
          menores x [] = []
          menores x (a:as) | a <= x = a : menores x as
                           | otherwise = menores x as
          maiores :: Ord a => a -> [a] -> [a]
          maiores x [] = []
          maiores x (a:as) | a <= x = a : maiores x as
                           | otherwise = maiores x as  -}
          menMai :: Ord a => a -> [a] -> ([a],[a])                --- Esta função é muito parecida ao unzip (nao me lembro)
          -- menMai x l = (menores x l, maiores x l)
          menMai x [] = ([],[])
          menMai x (a:as) = if (a>x) then (p,a:q) else (a:p,q)
            where (p,q) = menMai x as
                        

-- l = [7,2,5,3,9,8] , h = 7 , t = [2,3,5,8,9]
--   men = [2,5,3] mai = [9,8]
-- resultado deveria ser [2,3,5,7,8,9]

-- {x | x in N && x < 10} = {1,2,3,4,5,6,7,8,9}
-- def em compreensão         def em extensão

-- [x | x <- [1..10], mod x 2 == 0]


-- [x^2 | x <- [1..10], mod x 2 == 0]

-- Definição de listas por compreensão

-------------------------------------------------------------------------------

-- Definir uma função que ordena uma lista , em Haskell chama-se mergeSort

ordena4 [] = []
ordena4 l = juntaListasOrdenadas l1' l2'
    where l1 = sub-lista de l com (aproximadamente) 1/2 elementos  -- TPC : FAZER ISTO
          l2 = sub-lista de l com os restantes elementos           -- O DE CIMA
          l1' = ordena4 l1
          l2' = ordena4 l2
          juntaListasOrdenadas :: Ord a => [a] -> [a] -> [a]  -- esta função é conhecida como merge
          juntaListasOrdenadas [] b = b
          juntaListasOrdenadas a [] = a
          juntaListasOrdenadas (a:as) (b:bs) = if (a<b) then a:juntaListasOrdenadas as (b:bs)
                                               else b : juntaListasOrdenadas (a:as) bs  