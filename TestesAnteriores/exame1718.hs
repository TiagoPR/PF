import Data.List ((\\))

-- Exercicio 1

posicao :: [a] -> Int -> a
posicao l n = if (n > (length l-1)) then error "indice muito alto" else posicaoaux 0 l n
    where
        posicaoaux acc (h:t) n = if (acc == n) then h else posicaoaux (acc + 1) t n 

-- ou
{- 
index :: [a] -> Int -> a
index (h:t) n 
        | n == 0 = h
        | otherwise = index t (n - 1)
-}

-- Exercicio 2

data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao2 :: (Int,Int) -> [Movimento] -> (Int,Int) 
posicao2 p [] = p
posicao2 (x,y) (h:t) = case h of
    Sul -> posicao2 (x,y-1) t
    Norte -> posicao2 (x,y+1) t
    Este -> posicao2 (x+1,y) t
    Oeste -> posicao2 (x-1,y) t

-- Exercicio 3

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (h:t) = (f h == True) || (any' f t)

-- Exercicio 4

type Mat a = [[a]]
{- 
triSup :: Num a => Mat a -> Bool                                            I SURRENDER
triSup [[]] = False
triSup m = (quadrada m) && (triangular m)

quadrada :: Mat a -> Bool
quadrada [[]] = False
quadrada (h:t) = (length h) == (length (head (h:t)))

triangular :: Eq a => Mat a -> Bool
triangular [] = False
triangular (h:t) = triangularaux 0 t
    where
        triangularaux :: Int -> Mat a -> Bool
        triangularaux n (h:t) = if (h == n) then True && triangularaux (n+1) t else False



-- foldl (\acc x -> x !! acc == 0) -1 t
 -}

triSup :: (Num a,Eq a) => Mat a -> Bool
triSup matriz = all (\n -> all ((==) 0 . (!!) (matriz !! n)) [0..(n - 1)]) [1..(length matriz - 1)]

-- Exercicio 5

movimenta :: IO (Int,Int)
movimenta = mover (0,0)
    where
        mover (x,y) = do 
                    dir <- getChar
                    case dir of
                            'S' -> mover (x,y-1)
                            'N' -> mover (x,y+1)
                            'E' -> mover (x+1,y)
                            'O' -> mover (x-1,y)
                            otherwise -> return (x,y)


-- Exercicio 6

data Imagem = Quadrado Int | Mover (Int,Int) Imagem | Juntar [Imagem]

ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5), Quadrado 4, Mover (4,3) (Quadrado 2)])

ex2 = Juntar [Mover (5,5) (Quadrado 4), 
              Mover (5,6) (Quadrado 5),
              Mover (9,8) (Quadrado 2)]

-- a)

vazia :: Imagem -> Bool
vazia (Quadrado n) = False
vazia (Mover p i) = vazia i
vazia (Juntar (h:t)) = vazia h && vazialista t
    where
        vazialista [] = True
        vazialista (h:t) = vazia h && vazialista t

-- b)

maior :: Imagem -> Maybe Int
maior (Quadrado n) = Just n
maior (Mover p i) = maior i
maior (Juntar []) = Nothing
maior (Juntar i) = maximum (map maior i) 

{- ERRADO
maior (Quadrado n) = Just n
maior (Mover p i) = maior i
maior (Juntar (h:t)) = if (maior h > maior t) then maior h else maiorlista t
    where
        maiorlista [] = 0
        vazialista (h:t) = maior h && maiorlista t
 -}

-- c)

instance Eq (Imagem) where
    (==) i i1 =  null $ (quadPos i (0,0)) \\ (quadPos i1 (0,0)) 

quadPos :: Imagem -> (Int,Int) -> [(Int,(Int,Int))]
quadPos (Quadrado n) pos = [(n,pos)]
quadPos (Mover (a,b) img) (x,y) = quadPos img (x+a,y+b)
quadPos (Juntar imgs) pos = concatMap (\x -> quadPos x (pos)) imgs