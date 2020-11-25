{- 
funções infixas (operador "entre" os operandos) +, *, /, !!
ex: 4 + 5 , [1,2,3] !! 2
estas podem ser usadas em modo prefixo, englobando-as em parenteses ()
ex: (+) 4 5, (!!) [1,2,3] 2

funções prefixas (operador aparece "antes" dos operandos) div , mod , elem
ex: div 46 6 , mod 42 7, elem 'a' "haskell" 
estas podem ser usadas em modo infixo, englobando-as entre `` 
ex: 46 `div` 6, 'a' `elem` "haskell"
 -}


soma :: [Int] -> Int 
-- soma [] = 0
-- soma (h:t) = (+) h (soma t)
-- Voltando depois de ter visto o precorre
-- soma l = precorre (+) 0 l
soma = precorre (+) 0

concatena :: [[a]] -> [a]
-- concatena [] = []
-- concatena (h:t) = (++) h (concatena t)
-- concatena l = precorre (++) [] l
concatena = precorre (++) []

iSort :: Ord a => [a] -> [a]
-- iSort [] = []
-- iSort (h:t) = inserir h (iSort t)
-- iSort l = precorre inserir [] l
iSort = precorre inserir []
    where inserir x [] = [x]
          inserir x (y:ys)
            | x <= y = x : (y:ys)
            | otherwise = y : inserir x ys

-- Passo a passo

{- 
Caso geral:
precorre :: [a] -> b
precorre []    = 
precorre (h:t) = ... h (precorre t)            "Juntar a cabeça com a cauda já processada"
 -}

{- 
precorre :: ... -> ... -> [a] -> b
precorre junta v []    = ...
precorre junta v (h:t) = ... h (precorre junta v t)
 -}

{- 
precorre :: (... -> ...) -> b -> [a] -> b
precorre junta v []    = v                   -- v é do tipo b visto que v é o resultado as vezes
precorre junta v (h:t) = junta h (precorre junta v t)
 -}


precorre :: (a -> b -> b) -> b -> [a] -> b
precorre junta v []    = v                   
precorre junta v (h:t) = junta h (precorre junta v t)

-- A função precorre já existe em Haskell e chama-se foldr (fold right)

func l = foldr (:) [] l      -- Função identidade

func2 [] = []
func2 (h:t) = (:) h (func t) -- igual a (h : func t)

-- func [1,2,3] = 1 : func [2,3] = 1 : 2 : func [3] = [1,2,3]

-- soma = foldr (+) 0 

-- soma [1,2,3,4]
    -- = 1 + (2 + (3 + (4 + 0)))

-- Queremos um soma'  de tal forma que soma' [1,2,3,4] = (((0 + 1) + 2) + 3) + 4

soma' l = somaAcc 0 l
    where
        somaAcc ac [] = ac
        somaAcc ac (h:t) = somaAcc (ac + h) t  

{- 
precorreL :: ... -> ... -> [a] -> b
precorreL ... ... [] = ...
precorreL ... ... (h:t) = ...
 -}

{- 
precorreL :: ... -> ... -> [a] -> b
precorreL ... v [] = v
precorreL ... v (h:t) = precorreL ... ... t
 -}

 
{- 
precorreL :: ... -> ... -> [a] -> b
precorreL junta v [] = v
precorreL junta v (h:t) = precorreL junta ... t
 -}

precorreL :: ... -> ... -> [a] -> b
precorreL junta v [] = v
precorreL junta v (h:t) = precorreL junta (junta v h) t

soma' [1,2,3]
    = precorreL (+) 0 [1,2,3]
    = precorreL (+) ((+) 0 1) [2,3]
    = precorreL (+) ((+) ((+) 0 1) 2) [3]
    = precorreL (+) ((+) ((+) ((+) 0 1) 2) 3) []
    = ((+) ((+) ((+) 0 1) 2) 3)
    = ((0 + 1) + 2) + 3
