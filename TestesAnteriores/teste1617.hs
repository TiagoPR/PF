

-- Exercicio 1

type MSet a = [(a,Int)]

-- a)
-- cardMSet [('b',4),('a',2),('c',1)]
cardMSet :: MSet a -> Int
cardMSet [] = 0
cardMSet l = sum (map snd l)

-- b)

moda :: MSet a -> [a]
moda [] = [] 
moda (h:t) = [(fst (foldl (\acc x -> if snd x > snd acc then x else acc) h t))]

-- c)

converteMSet :: MSet a -> [a]
converteMSet [] = []
converteMSet ((a,b):t)
    | b > 0 = a : converteMSet ((a,b-1):t)
    | otherwise = converteMSet t

-- d)

addNcopies :: Eq a => MSet a -> a -> Int -> MSet a      -- pequenas duvidas aqui
addNcopies [] l v = [(l,v)]
addNcopies m l v = foldr (\(x,y) -> (:) (x,y + (if x == l then v else 0))) [] m


-- Exercicio 2

data SReais = AA Double Double | FF Double Double | AF Double Double | FA Double Double | Uniao SReais SReais

ex = Uniao (Uniao (AA 4.2 5.5) (AF 3.1 7.0)) (FF (-12.3) 30.0)

-- a)                     Bem easy

instance Show SReais where
    show (AA n n1) = "]" ++ show n ++ "," ++ show n1 ++ "["
    show (FF n n1) = "[" ++ show n ++ "," ++ show n1 ++ "]"
    show (AF n n1) = "]" ++ show n ++ "," ++ show n1 ++ "]"
    show (FA n n1) = "[" ++ show n ++ "," ++ show n1 ++ "["
    show (Uniao n n1) = "(" ++ show n ++ " U " ++ show n1 ++ ")"

-- b)

pertence :: Double-> SReais -> Bool
pertence vlue invalo = case invalo of
    AA n n1 -> (n < vlue) && (vlue < n1)
    FF n n1 -> (n <= vlue) && (vlue <= n1)
    AF n n1 -> (n < vlue) && (vlue <= n1)
    FA n n1 -> (n <= vlue) && (vlue < n1)
    Uniao n n1 -> (pertence vlue n) && (pertence vlue n1) 

-- c)

{- tira :: Double-> SReais -> SReais
tira vlue invalo = case invalo of
    AA n n1 -> 
    FF n n1 -> 
    AF n n1 -> 
    FA n n1 -> 
    Uniao n n1 ->  
 -}

-- Exercicio 3

data RTree a = R a [RTree a]

-- a) NAO PERCEBI NADA , SEGUIR EM FRENTE

percorre :: [Int] -> RTree a -> Maybe [a] 
percorre [] (R a _) = Just [a]
percorre _ (R a []) = Nothing
percorre (h:t) (R a r) | length r < h || null auxX = Nothing
                       | otherwise = Just (a:auxX)
    where aux = percorre t (r !! (h - 1))
          auxX = fromMaybe [] aux

-- b)
{- 
procura :: Eq a => a -> RTree a -> Maybe [Int]
procura x RTree a [] = if (x == a) then Just [a] else Nothing
 -}
procura :: Eq a => a -> RTree a -> Maybe [Int]
procura n (R a r) | n == a = Just []
                  | null r = Nothing
                  | otherwise = foldl (\acc num -> if procura n (r !! (num - 1)) == Nothing then acc else Just (num:fromMaybe [] (procura n (r !! (num - 1))))) Nothing [1..length r]