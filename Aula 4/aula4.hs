fatorial :: Integer -> Integer
-- 3! = 3*2*1
-- n! = 1 se n = 0
--    = n * (n-1)! se n>0

fatorial 0 = 1  	            -- Não podemos trocar as linhas devido à recursividade
fatorial n = n * fatorial (n-1) -- Pois não ia conseguir ler o fatorial de 0 pq nao saia da linha.

-- Alternativa relativamente aula 3

fatorial2 n = if n==0 then 1 else n*(fatorial (n-1))

--Função que dada uma lista de inteiros calcula a soma dos elementos
-- nome e tipo da função

soma :: [Int] -> Int
soma [] = 0
soma [x] = x
soma [x,y] = x + y
soma [x,y,z] = x + y + z

-- Porém não é boa programação por isso :

somal :: [Int] -> Int
somal [] = 0
-- Pode se comentar : somal [x] = x
somal l = (head l) + soma (tail l)

{-
somal [10,20,30,40]
= head [10,20,30,40] + somal(tail[10,20,30,40])=
= 10 + somal [20,30,40] =
= 10 + (head [20,30,40]) + somal (tail [20,30,40])
= 10 + 20 + somal ([30,40])
= 10 + 20 + (head [30,40] + somal (tail [30,40]))
= 10 + 20 + 30 + somal([40])
= 10 + 20 + 30 + 40 = 100


Listas em Haskell : 

 a) [] Lista Vazia
 b) (h : t) lista cujo primeiro elemento é h e os restantes são t.

-}
--Alternativa à linha 25 e 27
somal2 :: [Int] -> Int
somal2 [] = 0
somal2 (h:t) = h + somal2 t



        