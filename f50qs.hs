-- 1

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' n h = if (n <= h ) then n : enumFromTo' (n+1) h
    else []

-- 2

enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' ini dif fin
        | ini > dif && ini < fin = []
        | ini < dif && dif > fin = [ini]
        | otherwise = ini : enumFromThenTo' dif (dif + def) fin
    where
        def = (dif - ini)

-- 9

elem' :: Eq a => a -> [a] -> Bool 
elem' n [] = False
elem' n (h:t)
    | n == h = True
    | otherwise = elem' n t

-- 10

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n v
    | (n>0) = v : replicate' (n-1) v

-- 12

group' :: Eq a => [a] -> [[a]]
group' [] = [[]]
group' [x] = [[x]]
group' (h:t) = aux [h] t 
    where
        aux l [] = [l]
        aux (x:xs) (y:ys)
            | x == y = aux (x:y:xs) ys
            | otherwise = (x:xs) : aux [y] ys


-- 6 

take' :: Int -> [a] -> [a]
take' n [] = []
take' n (h:t) = aux2 n 0 (h:t)
    where
        aux2 n no [] = []
        aux2 n no (h:t) = if (n == no) then []
            else h : aux2 n (no + 1) t

-- muito mais simples

take2 0 l = []
take2 n (h:t) = h : take2 (n-1) t

-- 7

drop' 0 l = l
drop' n [] = []
drop' n (h:t) = drop' (n-1) t

-- 22 

difLista ::  Eq a => [a] -> [a] -> [a]
difLista [] l = []
difLista l [] = l
difLista (x:xs) (y:ys)
    | x == y = difLista xs ys 
    | otherwise = x : difLista xs (y:ys)

-- 26 e 27

unwords' :: [String] -> String
unwords' [] = ""
unwords' [x] = x
unwords' (h:t) = h ++ ' ' : unwords' t

unlines' :: [String] -> String
unlines' [] = "\n"
unlines' [x] = x ++ "\n"
unlines' (h:t) = h ++ '\n' : unlines' t

-- 30

algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t)
    | (h >= '0' && h <= '9') = h : algarismos t 
    | otherwise = algarismos t  

-- 34

iSort :: Ord a => [a] -> [a]             -- voltar a fazer +1
iSort [] = []
iSort (h:t) = insere h t'
    where
        t' = iSort t
        insere :: Ord a => a -> [a] -> [a]
        insere h [] = [h]
        insere h (x:xs)
            | (h <= x) = h : (x:xs)
            | otherwise = x : insere h xs

-- 38

converteMSet :: [(a,Int)] -> [a] 
converteMSet [] = []
converteMSet ((x,1):xs) = x : converteMSet xs
converteMSet ((x,y):xs) = x : converteMSet ((x,y-1):xs)

-- 42

partitionEithers :: [Either a b] -> ([a],[b])    -- Voltar a fazer
partitionEithers [] = ([],[])
partitionEithers (h:t) = case h of
        Left a -> ((a:x),y)
        Right b -> (x,(b:y))
    where
        (x,y) = partitionEithers t

-- 43

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (h:t) = case h of 
    Just a -> a : catMaybes t
    Nothing -> catMaybes t



-- Group again

group2 :: Eq a => [a] -> [[a]]
group2 [] = []
group2 [x] = [[x]]
group2 (h:t) = groupaux [h] t
    where
        groupaux l [] = [l]
        groupaux (h:t) (x:xs)
            | (h == x) = groupaux (h:x:t) xs
            | otherwise = (h:t) : groupaux [x] xs 


reverse' :: [a] -> [a]
reverse' [] = []
reverse' t = last t : reverse' (init t)

-- outra vez o partitioneithers

partitionEithers' :: [Either a b] -> ([a],[b]) 
partitionEithers' [] = ([],[])
partitionEithers' (h:t) = case h of
        Left a -> ((a:x),y)
        Right b -> (x,(b:y))
    where
        (x,y) = partitionEithers' t


iSort2 :: Ord a => [a] -> [a]
iSort2 [] = []
iSort2 (h:t) = insert h t'
    where
        t' = iSort2 t 
        insert h [] = [h]
        insert h (x:xs)
            | h <= x = h : (x:xs)
            | otherwise = x : insert h xs 

converteMSet'  :: [(a,Int)] -> [a]
converteMSet' [] = []
converteMSet' ((x,1):xs) = x : converteMSet' xs
converteMSet' ((x,y):xs) = x : converteMSet' ((x,y-1):xs)