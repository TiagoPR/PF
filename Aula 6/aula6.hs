-- Função junta : Junta duas listas "(++)"
-- Junta a lista [1,23] [2,5] = [1,23,2,5]
junta :: [a] -> [a] -> [a]
-- junta l1 l2 = ...
junta [] l2 = l2
junta (x:xs) l2 = x : (junta xs l2)

{- 
junta [1,2,3] [] = 1 : (junta [2,3] []) = 1 : 2 : (junta [3] []) = 1 : 2 : 3 : (junta [] []) = 1 : 2 : 3 : [] = [1,2,3]
-}

-- zip , unzip
zip' :: [a] -> [b] -> [(a,b)]
-- zip [1,2,3] "ola" = [(1,'o'),(2,'l'),(3,'a')]
-- zip' [] _ = []
-- zip' (_:_) [] = [] 
zip' (x:xs) (y:ys) = (x,y) : (zip' xs ys)  
zip' _ _ = []


unzip' :: [(a,b)] -> ([a],[b])
-- unzip [(1,'o'),(2,'l'),(3,'a')] = ([1,2,3],"ola")
unzip' [] = ([],[])
-- unzip' (h:t) = ...
-- unzip' ((x,y):t) = (x: ... , y: ... )

unzip' ((x,y):t) = (x :fst (unzip' t) , y:snd (unzip' t))

unzip' ((x,y):t) = (x : xs , y:ys)
    where (xs,ys) = unzip' t

-- x : 1 , y: '0' , t : [(2,'l'),(3,'a')]
-- unzip' [(2,'l'),(3,'a')] : ([2,3],"la")