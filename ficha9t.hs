import System.Random

-- Exercicio 1

-- a)

bingo :: IO ()
bingo = do putStrLn "Enter para ter novo numero"
           gerarBingo []

gerarBingo :: [Int] -> IO ()
gerarBingo l
    | (length l) == 90 = return ()
    | otherwise = do x <- getLine              -- receber o enter
                     p <- randomRIO (1,90)     -- gerar o numero
                     if elem p l               -- verificar se é um nº existente
                         then gerarBingo l     
                         else do print p 
                                 gerarBingo (p:l)
                         -- ou  else print p >> gerarBingo (p:l)

-- b)

mastermind :: IO ()
mastermind = do c <- gerarCodigo 
                mastermind' c

mastermind' :: String -> IO ()
mastermind' c = do cu <- readGuess
                let (ok,nok) = commpara c cu
                    if (ok == 4) then putStrLn "Ganhou"
                    else putStrLn show ok ++ " " ++ show nok >> mastermind' c -- esta a dar erro aqui n sei pq

gerarCodigo :: IO String                                     -- codigo que tens que adivinhar
gerarCodigo = do a <- randomRIO (0,9)
                 b <- randomRIO (0,9)
                 c <- randomRIO (0,9)
                 d <- randomRIO (0,9)
                 return show a ++ show b ++ show c ++ show d

readGuess :: IO String                                       -- lê o teu input
readGuess = do a <- getChar 
               b <- getChar
               c <- getChar
               d <- getChar
               if all isDigit [a,b,c,d] then return [a,b,c,d]
               else readGuess

compara :: String -> String -> (Int,Int)                     -- ve o que acertaste ou erraste
commpara cc cu = (checkOK cc cu, checkNotOK cc' cu')
    where
        (cc',cu') = unzip (filter (\(x,y) -> x /= y) (zip cc cu))

checkOK :: String -> String -> Int                            -- o valor das que acertaste na posicao
checkOK c n = foldl (\acc (f,s) -> if (f==s) then (acc + 1) else acc) 0 (zip c n)

checkNotOK :: String -> String -> Int                         -- o valor das que acertaste fora de posicao
checkNotOK [] _ = 0
checkNotOK (x:xs) c u
    | elem x cu = 1 + checkNotOK xs (delete x cu)
    | otherwise = checkNotOK xs cu