

-- Exercicio 1

-- a)

isSorted :: (Ord a) => [a] -> Bool
isSorted [x] = True 
isSorted (x:x1:xs) = if (x < x1) then isSorted (x1:xs) else False 

-- b)

inits :: [a] -> [[a]]
inits [] = [[]]
inits l = inits (init l) ++ [l]

-- Exercicio 2

maximumMB :: (Ord a) => [Maybe a] -> Maybe a
maximumMB [] = Nothing
maximumMB l = foldl (\acc x -> if x `maiorque` acc then x else acc) Nothing l
    where
        maiorque (Just a) (Just b) = a > b
        maiorque _ Nothing = True
        maiorque Nothing _ = False

-- Exercicio 3

data LTree a = Tip a | Fork (LTree a) (LTree a)

-- a)

listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork e d) = (listaLT e) ++ (listaLT d)

-- b)

instance Show a => Show (LTree a) where             -- falta cenas
    show x = mostrar x

mostrar :: Show a => LTree a -> String
mostrar (Tip a) = show a ++ "\n"
mostrar (Fork a b) = "." ++ mostrar a ++ "\n" ++ "." ++ mostrar b
        

-- Exercicio 4

l = [1,2,3]

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = foldl (\acc x -> max (sum x) acc) (sum l) (inits l)

-- Exercicio 5

type RelP a = [(a,a)]
type RelL a = [(a,[a])]
type RelF a = ([a], a->[a])

----------------------------------------------------------------------
convLP :: RelL a -> RelP a
convLP l = concat (map junta l)          -- mete os pares todos numa lista
    where junta (x,xs) = map (\y->(x,y)) xs  -- pega no x e junta com os numeros da lista xs
----------------------------------------------------------------------

-- a)

convPL :: (Eq a) => RelP a -> RelL a
convPL [(x,y)] = [(x,[y])]
convPL (h:t) = junta h (convPL t)
    where junta (a,b) l = if a `elem` map fst l
                        then map (\(c,d) -> if c == a then (c,b:d) else (c,d)) l
                        else (a,[b]) : l


-- b)

-- errado:
{- 
criaRelPint :: Int -> IO (RelP Int)
criaRelPint 0 = return []
criaRelPint n = do
                let (x,y) = (randomRIO (0,9), randomRIO (0,9))
                lista <- (x,y) ++ [criaRelPint (n-1)]
                return lista
 -}
-- solucao: 

criaRelPint :: Int -> IO (RelP Int)
criaRelPint 0 = return []
criaRelPint n = do 
    putStr "Introduz dois numeros (separados por um espaco): "
    (num1,num2) <- fmap (span (/= ' ')) getLine
    fmap ((read num1,read num2) :) $ criaRelPint (n - 1)