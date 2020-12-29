
-- 1

-- a)

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x:xs) = if (f x) then True
    else any' f xs

-- b)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (h:t) = (f x h) : zipWith' f xs t
zipWith' _ _ _ = [] 

-- f)

deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' _ _ [] = []
deleteBy' f x (h:t) = if (f x h) then t 
    else h : deleteBy' f x t

-- g)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f (h:t) = insert' f h (sortOn f t)
    where
        insert' f h [] = [h]
        insert' f h (x:xs)
            | (f h) <= (f x) = h : (x:xs)
            | otherwise = x : insert' f h xs

-- 2

type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- a)

selgrau :: Int -> Polinomio -> Polinomio
selgrau g p = filter (\x -> g == snd x) p

-- b) 

conta :: Int -> Polinomio -> Int
conta g p = length (selgrau g p)

-- ou

conta' g p = foldl (\acc m -> if (g == snd m) then acc + 1 else acc) 0 p

-- c)

grau :: Polinomio -> Int 
grau p = foldl (\acc x -> if (acc > snd x) then acc else snd x) 0 p

-- d)

deriv :: Polinomio -> Polinomio
deriv p = map (\(b,e) -> if e > 0 then (b * fromIntegral e, e - 1) else (0,0)) p   -- (*) tem de ser do mesmo tipo

-- e)

calcula :: Float -> Polinomio -> Float                             -- Não entendo a questão.
calcula n p = foldl (\acc (b,e) -> acc + b * (n ^ e)) 0 p          -- (^) pode ter tipos diferentes

-- f)

simp :: Polinomio -> Polinomio
simp p = filter (\(c,e) -> c /= 0) p

-- g)

mult :: Monomio -> Polinomio -> Polinomio  -- Tenho de fazer com funções de ordem superior
mult m [] = []
mult (c,e) ((c1,e1):t) = ((c*c1),(e+e1)) : mult (c,e) t

-- ou

mult1 :: Monomio -> Polinomio -> Polinomio                  -- CONSEGUIIIIIIIIIIIIIIIIIIIIIIIIII!
mult1 (c1,e1) p = map (\(c,e) -> (c*c1, e*e1)) p


-- h)

ordena :: Polinomio -> Polinomio
ordena p = reverse (sortOn (snd) p)


-- i)
{- 
normaliza :: Polinomio -> Polinomio            Não é necessário
normaliza [] = []
normaliza p = (norm (ordena p))
 -}
-- norm [] = []
-- norm p = foldl (\((c,e):p) (c1,e1) -> if (e == e1) then ((c+c1,e):p) else (c1,e1) : ((c,e):p) ) m p

-- k)

produto :: Polinomio -> Polinomio -> Polinomio
produto [] p = []
produto p1 p2 = concat (map (\m -> (mult m p2)) p1)

-- Exercicio 3

-- a) 
type Mat a = [[a]]

dimOK :: Mat a -> Bool
dimOK [x] = True
dimOK (h:x:t) = if ((length h) == (length x)) then dimOK (x:t)
    else False

-- Versão da aula

dimOK' (l:ls) = all(\x -> length x == length l) ls

-- b)

dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat (h:t)
    | (dimOK (h:t)) = (length (h:t),length h)

-- Versão da aula
dimMat' [] = (0,0)
dimMat' (l:ls) = (length (l:ls) , length l)

-- c) 

-- addMat [[1,2,3,1], [0,4,5,3], [0,0,6,4]] [[1,5,2,3],[6,2,4,1],[3,2,1,4]] = [[2,7,5,4],[6,6,9,4],[3,2,7,8]]

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] m = m 
addMat m  [] = m
addMat (x:xs) (y:ys) = (zipWith (+) x y) : addMat xs ys 

-- d) 

transpose :: Mat a -> Mat a

-- trocar as linhas por colunas, logo temos que obter o primeiro elemento de cada lista e por em uma lista , e depois dos da segunda,etc

-- transpose [[1,2,3], [0,4,5], [0,0,6]] = [[1,0,0],[2,4,0],[3,5,6]]

transpose [] = []
transpose ([]:_) = []        -- uma vez que vai chegar a ter a head : empty list
transpose m = (map head m) : (transpose (map tail m)) -- map (função) argumento

-- Versão da aula

transpose' [] = []
transpose' m = (map (head) m) : transpose' (map (tail) m) -- igual

-- e)

multMat :: Num a => Mat a -> Mat a -> Mat a
multMat m m1
        | (col1 == lin2) = [[sum (zipWith (*) l l2) | l2 <- (transpose m1)] | l <- m]
        | otherwise = error "incorrect dim"
    where
        (_ , col1) = dimMat' m
        (lin2 , _) = dimMat' m1 

