import System.Random

insereASorte :: a -> [a] -> IO [a]
insereASorte n [] = return [n]
insereASorte n l = do p <- randomRIO (0,length l - 1)  -- tem de ser um <- pq é um programa à direita
                      let (a,b) = splitAt p l          -- tem de ser um igual pq é uma função à direita
                      return (a ++ (n : b))

permutacao :: [a] -> IO [a]
permutacao [] = return []
permutacao (h:t) = do x <- permutacao t
                      insereASorte h x        -- nao precisas de return pq é um programa IO

wordCount :: String -> IO (Int,Int,Int) -- tem de ser IO pq o resultado vai alterar dependendo do que tem dentro do ficheiro
wordCount nome = do conteudo <- readFile nome
                    let linhas = length (lines conteudo)
                        palavras = length (words conteudo)
                        caracteres = length conteudo
                    return (linhas,palavras,caracteres)

-- wordCount "aula20.hs" =  (21,149,970)