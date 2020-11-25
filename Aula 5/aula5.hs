{- Tipos indutivos

data Maybe a = Nothing | Just a  -- Ao escrever data vamos caracterizar os varios padrões que existem

-- Maybe é um construtor (especie de funcão de tipos)

-- Nothing e Just chamam-se os construtores do tipo

Nothing :: Maybe Int
Just 2

Nothing :: [Maybe Int]
[Nothing] :: [Maybe Int]
[Just 2, Nothing]

Nothing :: Maybe [Int]
Just [2,3]

data Bool = True | False
-}

-- head :: [Int] -> Int

-- head :: [a] -> a -- Mais genérico, logo melhor

-- Todas as listas são definidas por estas duas linhas.

-- head [] = 
-- head (h:t) = 

-- head (h:t) = h

-- Ultimo elemento de uma lista (last)
ultimo :: [a] -> a
-- ultimo l = ???

-- ultimo [] = não está definido
-- ultimo (h:t) = -- h :: a , t :: [a]
ultimo (h:[])     = h 
ultimo (h:(x:xs)) = ultimo (x:xs)

-- testa se um elemento pertence à lista (elem)
pertence :: Eq a => a -> [a] -> Bool
-- pertence x l = ???
pertence x [] = False
pertence x (h:t) = 
    if x == h 
    then True 
    else pertence x t -- (x == h || pertence x t)