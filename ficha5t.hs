
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

-- c
{- 
grau :: Polinomio -> Int 
grau p =    -}

-- Exercicio 3

-- a) 
type Mat a = [[a]]

dimOK :: Mat a -> Bool
dimOK [x] = True
dimOK (h:x:t) = if ((length h) == (length x)) then dimOK (x:t)
    else False

-- b)

dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat (h:t)
    | (dimOK (h:t)) = (length (h:t),length h)

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

-- e)

multMat :: Num a => Mat a -> Mat a -> Mat a
multMat m m1
    | (length m) != (length m1) = error "incorrect dim"
    | otherwise = ...
