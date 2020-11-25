{-
Tipos : caracterizam os dados
	Tipos atómicos:
		Int : representar inteiros (-2^64 até 2^64)
		Integer : representar inteiros mas com mais precisão (não tem limite)
		Double : representar numeros com virgula mas com mais precisão
		Float : representar numeros com virgula : 1.3241 , -4
		Bool : Valores lógicos , True e False
		Char : Caracteres , 'a' , 'b' , 'A', '\''... '\\'
	Tipos não atómicos:
		Pares : (A,B) é o tipo dos pares ordernados em que a primeira componente é do tipo A e a segunda do tipo B
				O tipo de (3,3.14) é (Int,Float)
						   ('a',2) é (Char,Int)
						   ('a', (3,3.14)) é (Char, (Int,Float))
		Enúplos
-}
-- Enunciado : um programa (uma função) que recebe um numero inteiro e calcula o dobro desse numero

-- 1. Escolher um nome (e um tipo) para a função
dobro :: Int -> Int --Posso correr o codigo sem isto , e lê qualquer numero , mas nao especifica o tipo do dobro
-- 2. Definir a função

dobro x = 2 * x

-- Enunciado 2: uma função que recebe um par de inteiros e calcula o maior deles

--1. escolher um nome (e tipo)
maior :: (Int,Int) -> Int

--2. Definir a função

maior (x,y) = if x > y then x else y



