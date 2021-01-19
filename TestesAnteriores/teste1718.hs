

-- Exercicio 1

insert' :: Ord a => a -> [a] -> [a]
insert' n [] = [n]
insert' n (h:t) = if (n < h) then n : (h:t) else h : insert' n t

-- Exercicio 2
    
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (h:t) = case h of
    Nothing -> catMaybes t 
    Just a -> a : catMaybes t

-- Exercicio 3

data Exp a = Const a | Var String | Mais (Exp a) (Exp a) | Mult (Exp a) (Exp a)

e1 = (Mais (Var "x") (Mult (Const 3) (Const 4))) 

instance Show a => Show (Exp a) where
    show x = mostrar x

mostrar x = case x of 
    Const a -> show a
    Var a -> a
    Mais a b -> "(" ++ mostrar a ++ " + " ++ mostrar b ++ ")"
    Mult a b -> "(" ++ mostrar a ++ " * " ++ mostrar b ++ ")"


-- Exercicio 4

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f (h:t) = insert h (sortOn f t)
    where insert a [] = [a]
          insert a (x:xs) = if f a > f x then (x : insert a xs) else a : (x:xs)
{- 
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f (h:t) = insert' f h (sortOn f t)
    where
        insert' f h [] = [h]
        insert' f h (x:xs)
            | (f h) <= (f x) = h : (x:xs)
            | otherwise = x : insert' f h xs -}

-- Exercicio 5

-- a)

amplitude :: [Int] -> Int             -- FUCK SO PODE PASSAR PELA LISTA UMA VEZ
amplitude [] = 0
amplitude (h:t) = (foldl (\acc x -> if x > acc then x else acc) 0 (h:t)) - (foldl (\acc x -> if x < acc then x else acc) h (h:t))
{- 
amplitude ::  [Int] -> Int
amplitude [] = 0
amplitude l = mx - mn
    where (mx,mn) = foldl (\(a,b) n -> (if n > a then n else a,if n < b then n else b)) (head l,head l) l 
     -}

-- b) caga nisso é bue complicado o.O
{- 
parte :: [Int] -> ([Int],[Int]) 
parte [] = []
parte l =  -}

-- Exercicio 6

data Imagem = Quadrado Int | Mover (Int,Int) Imagem | Juntar [Imagem]

ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5), Quadrado 4, Mover (4,3) (Quadrado 2)])

-- a)

conta :: Imagem -> Int  -- FUNCIONAAAAAAAA!!
conta (Quadrado n) = 1
conta (Mover p n) = conta n
conta (Juntar (h:t)) = conta h + sum (map conta t)

-- b)
{- 
apaga :: Imagem -> IO Imagem
apaga  -}