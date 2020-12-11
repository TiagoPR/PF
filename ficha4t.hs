import Data.Char

-- Exercicio 1

-- b)

f :: [Int]
f = f' 1

f' :: Int -> [Int]
f' x
    | x <= 20 && x >= 1 = if (mod x 6 == 0) then x : (f' (x+1))
        else f'(x+1)
    | otherwise = []

-- 2

-- a)

dupl :: [Int]
dupl = [2^x | x <- [0..10]]

-- b)

execb = [(x,y)| x <- [1..5] , y<-[1..5], x + y == 6]

-- c)

execc = [[x | x <- [1..y]]| y <- [1..5]]

-- d)

execd = [[1 | x <- [1..y]] | y <- [1..5]]

-- e)

exece = [fact x | x <- [1..6]]

fact 0 = 1
fact n = n * fact (n-1)

-- 3
    
digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:t)
        | isDigit h = (h:x,y)
        | isAlpha h = (x,h:y)
        | otherwise = (x,y)
    where
        (x,y) = digitAlpha t 

-- 4

nzp :: [Int] -> (Int, Int,Int)
nzp [] = (0,0,0)
nzp (h:t)
        | h < 0 = (a+1,b,c)
        | h == 0 = (a,b+1,c)
        | otherwise = (a,b,c+1)
    where
        (a,b,c) = nzp t

-- 5

divMod' :: Integral a => a -> a -> (a,a)
divMod' _ 0 = error "divisao por 0"
divMod' x y
        | (x < 0 && y > 0) || (x>0 && y<0) = (-respos,rpos)
        | (x < 0 && y < 0) = (respos,rpos)
        | ((x - y) < 0) = (0,x)
        | otherwise = (res + 1, r)
    where
        (res,r) = divMod' (x-y) y
        (respos, rpos) = (divMod' (abs x) (abs y))
    
-- 6

fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (h:t) = h*10^(length t) + fromDigits t

fromDigits' [] acc = acc 
fromDigits' (h:t) acc = fromDigits' t (h + 10 * acc)

-- 7

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit [] = 0
maxSumInit (h:t) = max' t h h

-- Solução:
maxSumInit' :: (Num a ,Ord a) => [a] -> a
maxSumInit' [] = 0
maxSumInit' (h:t) = max' t h h

max' [] _ maxi = maxi
max' (h:t) total maxi = 
    if maxi > (total + h) then max' t (total + h) maxi
    else max' t (total + h) (total + h)

-- 8

fib :: Int -> Int 
fib n = fib' n 0 1
    where
        fib' :: Int -> Int -> Int -> Int
        fib' 0 acc_n acc_nplus = acc_n
        fib' n acc_n acc_nplus = fib' (n-1) (acc_nplus) (acc_nplus + acc_n)
