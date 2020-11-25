-- Exercicio 12 da 50q

iguaisNoPrincipio :: Eq a => [a] -> [a]
-- iguaisNoPrincipio [1,1,2,3,11,1] = [1,1]
iguaisNoPrincipio [x] = [x]
iguaisNoPrincipio (x:x2:xs) 
    | x == x2 = x : iguaisNoPrincipio (x2:xs)
    | otherwise = [x]

group' [] = []
group' l = p : group' (drop (length p) l)
    where
        p = iguaisNoPrincipio l


    

----------------------------------------------------

-- Ver os apontamentos da aula 12

{-  Igualdade de funções
  f == g se e só se f x == g x   -- duas funcoes sao iguais quando dão sempre os mesmos valores
 -}

-- f x y == (f x) y

{- max :: a -> (a -> a )
 (max 3) 4 = 4

max :: (a,a) -> a
 max (3,4) = 4 -}



-- Igualdade de funções
--   f == g se e só se f x == g x

{- f x = 2 * x 
f x = (2*) x       é o mesmo que  f = 2*
       g
    
outra vez a mesma estratégia...

dobros l = map (2*) l 
            f    x   y  = (f x) y  


(dobros) l = (map (2*)) l 
   fun l = fun'        l -}

-------------------------------------------------
-- dobros :: [Int] -> [Int]
{- dobros l = map (2*) l

dobros [1,2,3]
 = map (2*) [1,2,3]

dobros = map (2*)

dobros [1,2,3]
 = map (2*) [1,2,3] -}

dobros = map (2*)

filter' :: (a -> Bool) -> [a] -> [a]
-- selecciona uma sublista
filter' teste []    = []
filter' teste (h:t) = if (teste h) then h : filter' teste t
    else filter' teste t


takeWhile' :: (a -> Bool) -> [a] -> [a]
-- selecciona um prefixo

takeWhile' teste []    = []
takeWhile' teste (h:t) = if (teste h)
                         then h : takeWhile' teste t 
                         else []

-- filter     odd [1,2,3,4,5] = [1,3,5]
-- takeWhile' odd [1,2,3,4,5] = [1]