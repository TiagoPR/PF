--1
eft::Int -> Int -> [Int]
eft x y | x==y = [x]
        | x<y = x: eft (x+1) y
        | otherwise= []

--2
eFthen:: Int -> Int -> Int -> [Int]
eFthen x y z | ((x<y)&&(y+(y-x)<=z)) = x: eFthen y (y+(y-x)) z 
             | (x>y)&&(y+(x-y)>=z) = x: eFthen y (y-(x-y)) z
             | otherwise= [x] 



dois::Int -> Int -> Int -> [Int]
dois x y z | x==z = [x]
           | x<z = x: dois y (y+(y-x)) z
           | x>=z = x:dois y (y-(x-y)) z
           | otherwise =[x]



--3
maismais::[a]->[a]->[a]
maismais [] l= l
maismais l []= l
maismais (h:t) l= h:(maismais t l)

--4
posicao::[a]->Int->a
posicao (h:t) 0 = h
posicao (h:t) x = posicao t (x-1)

--5
minhareverse::[a]->[a]
minhareverse []=[]
minhareverse (h:t) = (minhareverse t)++[h]

--6
minhatake:: Int->[a]->[a]
minhatake n []= []
minhatake 0 (h:t)= []
minhatake n (h:t)= h:(minhatake (n-1) t)

--7
minhadrop::Int->[a]->[a]
minhadrop n []= []
minhadrop 0 (h:t)= (h:t)
minhadrop n (h:t)= (minhadrop (n-1) t)

--8
minhazip:: [a]-> [b]-> [(a,b)]
minhazip [] _ =[]
minhazip _ [] =[]
minhazip (h:t) (x:y)= (h,x): (minhazip t y)

--9
minhaelem:: Eq a =>a-> [a]->Bool
minhaelem x []= False
minhaelem x (h:t) = if x==h then True else (minhaelem x t) 

--10
minhareplicate:: Int-> a-> [a]
minhareplicate 0 _ = []
minhareplicate n x = x: (minhareplicate (n-1) x)

--11
minhaintersperce:: a-> [a]->[a]
minhaintersperce x []= []
minhaintersperce x [h]=[h]
minhaintersperce x (h:t)= h:x:(minhaintersperce x t)

--12
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (h:t) = takeWhile (==h) (h:t) : group' (dropWhile (==h) (h:t))
 
--13
miconcat :: [[a]] -> [a]
miconcat [] = []
miconcat (h:t) = h ++ miconcat t

--14
miinits::[a]->[[a]]
miinits []=[[]]
miinits l = miinits (init l)++ [l]

--15
mitails::[a]->[[a]]
mitails []= [[]]
mitails (h:t) = (h:t): (mitails t)

--16
isPrefix:: Eq a=> [a]-> [a]-> Bool
isPrefix [] _= True
isPrefix _ [] = False
isPrefix (h:t) (x:y)= if h==x then isPrefix t y else False

--17
issuffix::Eq a=> [a]-> [a]-> Bool
issuffix [] _ = True
issuffix _ [] = False
issuffix l1 l2 = if isPrefix (reverse l1) (reverse l2) then True else False

--18
isSubseq::  Eq a => [a] -> [a] -> Bool
isSubseq [] [] = True
isSubseq [] _ = True
isSubseq _ [] = False
isSubseq (x:xs) (y:ys) | x==y = isSubseq xs ys
                               | otherwise = isSubseq (x:xs) ys

--19
indices:: Eq a=> a -> [a]-> [Int]
indices x l = indicesAux x l 0

indicesAux::Eq a=> a-> [a]-> Int-> [Int]
indicesAux _ [] n = []
indicesAux x (h:t) n = if x==h then [n] ++ indicesAux x t (n+1) else indicesAux x t (n+1)

--20
minub:: Eq a=> [a]-> [a]
minub []=[]
minub (h:t)= if elem h t then minub t else h: minub t

--21
midelete:: Eq a =>a -> [a]->[a]
midelete x []= []
midelete x (h:t)= if x==h then t else h:(midelete x t)

--22
barra::Eq a =>[a] -> [a]->[a]
barra [] l = []
barra l [] = l
barra l (h:t) = barra (apaga1 h l) t

apaga1:: Eq a =>a -> [a]->[a]
apaga1 x []= []
apaga1 x (h:t)= if x==h then t else h:(apaga1 x t)



--23
miunion:: Eq a => [a]->[a]->[a]
miunion [] l= l
miunion l []= l
miunion (h:t) (x:xs) = if h==x then h: (miunion t xs) else h: miunion t (x:xs)


--24
mintersect::Eq a => [a]->[a]->[a]
mintersect [] _ = []
mintersect _ [] = [] 
mintersect (h:t) (x:y)= if h==x then h:(mintersect t (x:y)) else (mintersect t y)

--24 
intersect1:: Eq a => [a]->[a]->[a]
intersect1 [] _ = []
intersect1 _ []=[]
intersect1 (h:t) l = if elem h l then h:intersect1 t l else intersect1 t l



--25
minsert:: Ord a=> a-> [a]->[a]
minsert x [] = [x]
minsert x (h:t)= if x<=h then x:h:t else h:(minsert x t)

--26
miunwords:: [String]-> String
miunwords []=[]
miunwords (h:t)= h++ " " ++ miunwords t

--27
miunlines::[String]-> String
miunlines []=[]
miunlines (h:t)= h++ "\n"++ miunlines t

--28
pMaior::Ord a=>[a]->Int
pMaior [x] = 0
pMaior (h:t) = if h == aux (h:t)
               then 0
               else 1 + pMaior t
 where aux [x] = x
       aux (x:y:xs)  = if x>y
                       then aux (x:xs)
                       else aux (y:xs)

--29
miTemRep:: Eq a => [a]-> Bool
miTemRep []= False
miTemRep (h:x:t) | h==x = True   
                 | elem h t =True
                 | elem x t = True
                 | otherwise= miTemRep t

--30
mialgarismos::[Char]-> [Char]
mialgarismos []=[]
mialgarismos (h:t)= if ((h>= '0') && (h<= '9')) then h:(mialgarismos t) else mialgarismos t

--31
posImpares1:: [a]-> [a]
posImpares1 []= []
posImpares1 [x]= []
posImpares1 (h:x:t)= x: posImpares1 t 

--32
posPares1 :: [a]-> [a]
posPares1 []=[]
posPares1 [x]= [x]
posPares1 (h:x:t)= h: posPares1 t

--33
misSorted::Ord a=> [a]-> Bool
misSorted []= True
misSorted (h:x:t)= if h<=x then misSorted (x:t) else False

--34
miSort::Ord a=> [a]-> [a]
miSort []= []
miSort [x]=[x]
miSort (h:t)= aux h (miSort t)

aux :: Ord a => a -> [a]-> [a]
aux x []= [x]
aux x (y:ys)= if x <=y then x:y:ys else y: aux x ys

--35
menor::String->String-> Bool
menor [] [] = True
menor [] _= True
menor _ []= False
menor (x:xs) (y:ys) | x<y=True
                    | x>y = False
                    | otherwise= menor xs ys

--36
elemMset:: Eq a => a-> [(a,Int)]->Bool
elemMset x []= False
elemMset x ((a,b):t)= if x==a then True else elemMset x t 

--37
lengthMset1:: [(a,Int)]-> Int
lengthMset1 []=0
lengthMset1 ((x,b):t)= b +(lengthMset1 t)

--38
converteMSet :: [(a,Int)] -> [a] 
converteMSet []=[]
converteMSet ((a,1):t)= a:converteMSet t
converteMSet ((a,b):t)= [a] ++ converteMSet ((a,b-1):t) 

--39
insereMset:: Eq a => a -> [(a,Int)]->[(a,Int)]
insereMset x []= [(x,1)]
insereMset x ((a,b):t)= if x==a then ((a,b+1):t) else (a,b):insereMset x t

--40
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((a,1):t)= if x==a then t else (a,1): removeMSet x t  
removeMSet x ((a,b):t)= if x== a then ((a,b-1):t) else (a,b): removeMSet x t

--41
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet l = aux 1 l
 where aux i [x] = [(x,i)]
       aux i (x:y:xs) = if x==y
                        then aux (i+1) (x:xs)
                        else (x,i):aux 1 (y:xs)

--42
partitionEithers1 :: [Either a b] -> ([a],[b])
partitionEithers1 [] = ([],[])
partitionEithers1 (e:es) = case e of
                                Left a -> ((a:x),y)
                                Right b -> (x,(b:y)) 
  where (x,y) =  (partitionEithers1 es)

--43
catMaybes1 :: [Maybe a] -> [a]
catMaybes1 [] = []
catMaybes1 ((Just x):xs) = x : catMaybes1 xs
catMaybes1 ((Nothing):xs) = catMaybes1 xs

data Movimento = Norte | Sul | Este | Oeste
                 deriving Show 
--44
posicao2 :: (Int,Int) -> [Movimento] -> (Int, Int)
posicao2 (x,y) [] = (x,y)
posicao2 (x,y) (Norte:hs) = posicao2 (x,y+1) hs
posicao2 (x,y) (Sul:hs) = posicao2 (x,y-1) hs
posicao2 (x,y) (Este:hs) = posicao2 (x+1,y) hs
posicao2 (x,y) (Oeste:hs) = posicao2 (x-1,y) hs


--45
caminho :: (Int,Int) -> (Int,Int) -> [Movimento] 
caminho (a,b) (x,y) | ((a==x)&&(b==y)) = []
                    | a>x = Oeste: caminho (a-1,b) (x,y)
                    | a<x = Este: caminho (a+1,b) (x,y)
                    | b>y = Sul : caminho (a,b-1) (x,y)
                    | otherwise= Norte : caminho (a,b+1) (x,y)


--46
vertical::[Movimento]->Bool
vertical [] = True
vertical (x:xs) = case x of
                  Norte-> vertical xs
                  Sul->vertical xs
                  _->False

data Posicao = Pos Int Int 
               deriving Show
--47
maisCentral:: [Posicao]-> Posicao
maisCentral [x]= x 
maisCentral ((Pos a b):(Pos h t):s) | dist (a,b) > dist (h,t) = maisCentral ((Pos h t):s)
                                     | otherwise= maisCentral ((Pos a b):s)


dist:: (Int,Int)-> Float
dist (x,y)= sqrt (fromIntegral ((x^2)+(y^2)))

--48
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos x [] = []
vizinhos (Pos x y) ((Pos w z):xs) | (x == (w-1)) || (x == (w +1)) || (y == (z-1)) || (y== (z+1)) = (Pos w z ) : (vizinhos (Pos x y) xs)
                                  | otherwise = vizinhos (Pos x y) xs

--49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada ((Pos a b):(Pos c d):t) | b==d = mesmaOrdenada ((Pos a b):t)
                                       | otherwise = False


data Semaforo = Verde | Amarelo | Vermelho
                deriving Show

--50
intersecaoOk :: [Semaforo] -> Bool
intersecaoOk [] = True
intersecaoOk l|(aux1 l) > 1 = False
              |otherwise = True

aux1 :: [Semaforo] -> Int
aux1 [] = 0
aux1 (h:t) = case h of 
                  Verde -> 1 + (aux1 t)
                  Amarelo -> 1 + (aux1 t)
                  Vermelho -> (aux1 t)

























