
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