import Data.Char -- Exercicio 8 apenas

-- Exercicio 1

-- a)
perimetro :: Double -> Double
perimetro r = 2 * pi * r   -- A funcionar

-- b)
dist :: (Double , Double) -> (Double , Double) -> Double
dist (x1, y1) (x2, y2) = sqrt((((x1-x2)^2)+ (y1-y2)^2))

-- c)
primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)

-- d)
multiplo :: Int -> Int -> Bool
multiplo m n = (mod m n) == 0

-- Exercicio 2

-- a)
-- *Aprender as syntaxes das operações no Haskell* 
nRaizes a b c = 
    if (b^2 - 4 * a * c) > 0
    then 2
    else if (b^2 - 4 * a * c) == 0
    then 0
    else 1

-- Outra maneira de fazer , declarando a variavel det
nRaizes2 a b c =
        if (det) > 0
        then 2
        else if (det) == 0 
        then 0
        else 1
    where
        det = b^2 - 4 * a * c

nRaizes3 a b c               -- Em guardas não se põe iguais!!!! Fuck sake
        | (det > 0) = 2
        | (det == 0) = 1
        | otherwise = 0
    where
        det = b^2 - 4 * a * c

--- b)
raizes a b c
        |(nRaizes a b c == 2) = [(-b+root)/2*a,(-b-root)/2*a]
        |(nRaizes a b c == 1) = [-b/2*a]
        |otherwise = []
    where
        root = sqrt(b^2-4*a*c)

--- Exercicio 3 

-- a) 

type Hora = (Int ,Int)

valida :: Hora -> Bool
valida (h,m) = h >= 0 && h <= 23 && m>=0 && m<=59

depois :: Hora -> Hora -> Bool
depois (h1,m1) (h2,m2) =
    if(h1>h2) then True
    else if (h1 == h2) && m1>m2 then True
    else False

-- Outra maneira

depois2 (h1,m1) (h2,m2) = h1 > h2 || (h1 == h2) && m1 > m2

--- c)

hora2min :: Hora -> Int
hora2min (h,m) = h*60+m 

--- d)

min2hora :: Int -> Hora          ---- Falta-me ter os minutos
min2hora m = (m `div` 60,0)

--- e)

difhora :: Hora -> Hora -> Int
difhora h1 h2 = abs(min1 - min2)
    where
        min1 = hora2min h1
        min2 = hora2min h2

--- f)

addMin :: Int -> Hora -> Hora
addMin m (H h m2) = if (n >= 60) then (H (h+h1) m3)
        else (H h (n))
    where n = m2 + m
          h1 = div (m+m2) 60 
          m3 = mod (m+m2) 60 

--- Exercicio 4

data Hora2 = H Int Int deriving (Show,Eq)

valida' :: Hora2 -> Bool
valida' (H h m) = valida(h,m)

--- Exercicio 5

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

-- a)

next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

next2 :: Semaforo -> Semaforo
next2 s  
    | (s == Verde) = Amarelo
    | (s == Amarelo) = Vermelho
    | (s == Vermelho) = Verde

next3 s = case s of
    Verde -> Amarelo
    Amarelo -> Vermelho
    Vermelho -> Verde

-- b)

stop :: Semaforo -> Bool
stop s = case s of
    Verde -> False
    Vermelho -> True
    Amarelo -> False

-- c)

safe :: Semaforo -> Semaforo -> Bool
safe x y =
    if stop x || stop y == True
        then False
        else True


--- Exercicio 6

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

-- a)
posx :: Ponto -> Double 
posx (Cartesiano x y) = x
posx (Polar r a) = r * cos a

-- b)
posy (Cartesiano x y) = y
posy (Polar r a) = r * sin a

-- c)
raio p = case p of
    (Cartesiano x y) -> sqrt (x^2 + y^2)
    (Polar r a) -> r

-- d)
angulo p = case p of
    (Cartesiano x y) -> atan(y/x)
    (Polar r a) -> a

-- / -> Divisão com nº virgulas
-- div -> Divisão de nº inteiros

-- Exercicios extra

cartesiano2polar :: Ponto -> Ponto
cartesiano2polar (Cartesiano x y) = (Polar (sqrt(x^2 + y^2)) (atan (y/x)))
cartesiano2polar p = p -- Os outros casos devolvem o próprio ponto visto que é Polar already.

polar2cartesiano (Polar r a) = (Cartesiano (r*cos a)  (r*sin a))
polar2cartesiano p = p

raio' p = r where
    (Polar r a) = cartesiano2polar p   -- Pergunta : Porque não dá ao contrário?

-- e)

-- dist (Cartesiano x y) (Cartesiano a b) = sqrt ((x-a)^2 + (y-b)^2)
dist2 p1 p2 = sqrt((x-a)^2 + (y-b)^2) where
    (Cartesiano x y) = polar2cartesiano p1
    (Cartesiano a b) = polar2cartesiano p2

-- Exercicio 7

data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

-- a)

poligono :: Figura -> Bool
poligono (Circulo _ _) = False
poligono _ = True 

poligono2 f = case f of
    (Circulo _ _) -> False
    otherwise -> True

-- b)

vertices :: Figura -> [Ponto]
vertices f = case f of
    (Triangulo p1 p2 p3) -> [p1,p2,p3]
    (Rectangulo p1 p2) -> [p1,p2,p3,p4] where
        p3 = (Cartesiano (posx p1) (posy p2))   -- Explicação em escrito
        p4 = (Cartesiano (posx p2) (posy p1))   -- Explicação em escrito
    (Circulo _ _) -> []

-- c)

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist2 p1 p2
        b = dist2 p2 p3
        c = dist2 p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron

area (Rectangulo p1 p2)=
    let p3 = Cartesiano (posx p1) (posy p2)
        base = dist2 p2 p3
        altura = dist2 p1 p3
    in base * altura

area (Circulo _ r) = pi * r^2

-- d)
perimetro2 :: Figura -> Double
perimetro2 (Circulo _ r) = (2 * pi * r)

perimetro2 ( Rectangulo p1 p2) = 
    let p3 = Cartesiano (posx p1) (posy p2)
        base = dist2 p2 p3
        altura = dist2 p1 p3
    in base * 2 +  altura * 2

perimetro2 (Triangulo p1 p2 p3) = (a + b + c) where
    a = dist2 p1 p2
    b = dist2 p2 p3
    c = dist2 p3 p1

-- Exercicio 8

-- Explicação em escrito

-- a)
-- ord :: Char -> Int    chr :: Int -> Char
-- Temos de fazer "import Data.Char"

-- 97 - 122 é td minuscula

isLower' :: Char -> Bool 
isLower' c = if (ord c) >= 97 && (ord c) <= 122
    then True else False

-- Alternativa

isLower2 c = (ord c) >= 97 && (ord c) <= 122

-- b)

-- 48 ao 57 são dígitos
isDigit' :: Char -> Bool
isDigit' c = ((ord c) >= 48 && (ord c) <= 57)


-- 65 ao 90

isUpper' c = (ord c) >= 65 && (ord c) <= 90 -- Exercicio extra

-- c)
isAlpha' :: Char -> Bool
isAlpha' c = (isLower' c) || (isUpper' c)  -- Se é letra

-- d)
toUpper' :: Char -> Char
toUpper' c = chr((ord c) - 32)

-- e)
intToDigit' :: Int -> Char
intToDigit' d = chr (d + 48)

-- f)
digitToInt' :: Char -> Int
digitToInt' c = (ord c) - 48




