
pmaior :: Ord a => [a] -> Int

-- pmaior [1,3,2,7,4,8,1] == 5 (posicao 5)
pmaior [] = error "nao esta definido para listas vazias"
pmaior [x] = 0
pmaior (h:t) = if h > (sel t p) then 0
               else p+1
    where p = pmaior t
-- pmaior (h:t) = if h > "o elemento de t que está na posição p"
--    where p = pmaior t

-- p = pmaior [3,2,7,4,8,1] == 4

pmaior2 l = snd (pmaiorAux l)

sel :: [a] -> Int -> a
-- sel [1,2,3] 0 = 1  esta funcao existe em haskell e chama-se !!
sel [] p = error "index too large"
sel (h:t) p = if (p == 0) then h
    else sel t (p-1)

-- nos tipos     restrições => tipo
pmaiorAux :: Ord a => [a] -> (a,Int) -- Se ficar em comentario o ghc descobre o tipo
pmaiorAux [x] = (x,0)
pmaiorAux (h:t) = if h > m then (h,0) else (m,p+1)
    where (m,p) = pmaiorAux t
