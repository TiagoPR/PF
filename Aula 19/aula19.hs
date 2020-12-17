import System.Random     -- Nao funciona nao sei pq // Já sei o pq , (download stack) e dar run com -- stack ghci <ficheiro.hs>

-- elem :: Eq a => a -> [a] -> Bool        -- recomendaçao do stor: verificar os standard prelude

{- 
dialogo :: String 
dialogo = 
   -}  

-- IO -> input output

-- x :: IO a

-- (na consola) import System.IO 


dialogo :: IO () -- () -> significa devolver nada
dialogo = do putStr "Nome? "
             x <- getLine
             putStr ("Boa tarde " ++ x ++ "\n")

randomList :: Int -> (Int,Int) -> IO [Int] 
-- randomList n (i,s) produz uma lista com n numeros entre i e s
randomList 0 (i,s) = return []
randomList n (i,s) = do x <- randomRIO (i,s)
                        xs <- randomList (n-1) (i,s)
                        return (x:xs)