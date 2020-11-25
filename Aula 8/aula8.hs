
-- Definir uma função que ordena uma lista , em Haskell chama-se iSort(Insertion Sort)
ordena :: Ord a => [a] -> [a]       -- Quando vamos comparar coisas temos que adicionar restrições com o =>
ordena [] = []
ordena (h:t) = insere h t'
    where t' = ordena t

insere :: Ord a => a -> [a] -> [a]
insere x [] = [x]
insere x (h:t) = if (x<h) then (x:h:t)
    else (h:insere x t)

-- insere x l assume que l está ordenada
-- retorna a lista ordenada com x e os elementos de l

-- ordena [7,2,5,3,9,8] == [2,3,5,7,8,9]
-- h = 7, t' = [2,3,5,8,9]
{- ordena [] = ...
ordena (h:t) = ...  -}

---------------------------------------------------------------------------------------------------------------------
-- Definir uma função que ordena uma lista , em Haskell chama-se minSort(Minimum Sort)

-- ordena2 [7,2,5,3,9,8] = h: ordena t
--            h == 2 (minimo de l)
--            t == [7,5,3,9,8]
ordena2 [] = []
ordena2 l = h:(ordena t)
    where h = minimo l -- h é o menor elemento de l 
          t = remove h l -- t é a lista l removendo h

minimo [] = error "Empty list"
minimo [x] = x
minimo (h:t) = min h (minimo t)

remove :: Eq a => a -> [a] -> [a]
-- remove um elemento que EXISTE na lista
-- remove x [] = []
remove x [y] = [] -- como é uma função auxiliar o x vai sempre pertencer a [y], visto que no contexto do exericicio vamos remover o elemento 
-- mais pequeno da lista
remove x (h:t) = if (x==h) then t 
    else h:(remove x t)

-- ou

remove1 x [] = []
remove1 x (h:t) = if (x==h) then remove1 x t -- elimina todos os elementos iguais na lista, nos nao queremos isso no contexto da função
    else h:(remove1 x t)

----------------------------------------------------------------------------------------------------
{-
ordena [] = []
ordena (h:t) = ???
    where men = lista dos elementos de t que são menores ou iguais (<=) a h
        mai = lista dos elementos de t que são maiores (>) que h
 -}

-- Definir uma função que ordena uma lista , em Haskell chama-se quickSort
ordena3 [] = []
ordena3 (h:t) = (ordena3 men) ++ (h : (ordena3 mai))
    where men = menores h t
          mai = maiores h t
          menores :: Ord a => a -> [a] -> [a]
          menores x [] = []
          menores x (a:as) | a <= x = a : menores x as
                           | otherwise = menores x as
          maiores :: Ord a => a -> [a] -> [a]
          menores x [] = []
          maiores x (a:as) | a <= x = a : maiores x as
                           | otherwise = maiores x as 
                        

-- l = [7,2,5,3,9,8] , h = 7 , t = [2,3,5,8,9]
--   men = [2,5,3] mai = [9,8]
-- resultado deveria ser [2,3,5,7,8,9]

{-
remove 5 [2,3] = if ( 5 == 2) then [3]
    else 2 : (remove 5 3)

= 2 : [] = [2]
 -}