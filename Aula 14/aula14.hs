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
--    = 1 : filter odd [2,3,4,5] = 1 : filter odd [3,4,5] = 1 : 3 : filter odd [4,5] = 1:3:5: filter odd [] = 1 : 3 : 5 : [] = [1,3,5]

-- takeWhile' odd [1,2,3,4,5] = [1]
--    = 1 : takeWhile odd [2,3,4,5] = 1 : [] = [1]

dropWhile_ :: (a -> Bool) -> [a] -> [a]
dropWhile_ teste [] = []
dropWhile_ teste (h:t) = if teste h then dropWhile_ teste t
    else (h:t)


zip_ :: [a] -> [b] -> [(a,b)]
zip_ [] _ = []
zip_ _ [] = []
zip_ (x:xs) (y:ys) = (x,y) : zip_ xs ys 

zipWith_ :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith_ f [] _ = []
zipWith_ f _ [] = []
zipWith_ f (x:xs) (y:ys) = (f x y) : zipWith_ f xs ys

----------------------------------------------------------------------------------------------------------------------------------------

type Matriz = [Linha]
type Linha = [Int]

m1,m2,m3 :: Matriz

m1 = [[1,2,3]
     , [4,5,6]
     , [7,8,9]]

m2 = [[3,0,4]
     , [1,1,1]
     , [0,0,0]]
    
m3 = [[4,2,7]
     , [5,6,7]
     , [7,8,9]]


-- f x y é o mesmo que (f x) y

somaM :: Matriz -> Matriz -> Matriz
-- somaM [] [] = []
-- somaM [] m2 = []
-- somaM (l:ls) [] = []
-- somaM (l:ls) (k:ks) = (somaLinhas l k) : somaM ls ks 
-- somaM m1 m2 = zipWith_ somaLinhas m1 m2
somaM = zipWith_ (zipWith_ (+))

-- somaLinhas :: Linha -> Linha -> Linha
-- somaLinhas [] [] = []
-- somaLinhas (x:xs) (y:ys) = (x + y) : somaLinhas xs ys
-- somaLinhas l1 l2 = zipWith_ (+) l1 l2
-- somaLinhas = (zipWith_ (+))

-- ALTERNATIVA AO somaM na linha 71 e 66

-- (somaLinhas l1) l2 = ((zipWith_ (+) ) l1) l2
    --------------       --------------------
--      f1         l2 =            f2        l2

-- é o mesmo que escrever isto

-- somaLinhas l1 = ((zipWith_ (+) l1)
    ---------      --------------
--    f1         =    f2        

-- é o mesmo que escrever isto

-- somaLinhas = zipWith_ (+)


