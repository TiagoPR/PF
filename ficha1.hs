import Data.Char

perimetro x = 2 * pi *x 

-------------------

dist x y = sqrt((x!!0-y!!0)^2+ (x!!1 - y!!1)^2)
dist2 (x1, y1) (x2, y2) = sqrt((((x1-x2)^2)+ (y1-y2)^2))

-------------------
primUlt :: [a] -> (a,a)
primUlt x = (head x,last x)

-------------------

multiplo x y = do
    let z = mod x y
    if z /= 0
    then print( show(x) ++ " is not multiple of "++ show(y) )
    else print( show(x)++" is multiple of " ++ show(y))

-------------------
truncaImpar :: [a] -> [a]
truncaImpar x = do
    let y = length x
    if mod y 2/=0
    then tail x
    else x
--------------------
max2 x y= max x y
--------------------
max3 max2 z = max max2 z
--------------------
nRaizes a b c = do
    print(((-b + sqrt((b^2)-4*a*c))/(2 *a)))
    print(((-b - sqrt((b^2)-4*a*c))/(2 *a)))

nRaizes2 a b c = do
    if ((b^2)-4*a*c)>0
    then 2
    else if ((b^2)-4*a*c)<0
    then 0
    else 1 

--------assign de variaveis mais tarde = where
-----------------------------------------x = y

nRaizes3 a b c 
    |i > 0 = 2
    |i == 0 =1
    |otherwise = 0
    where i = ((b^2)-4*a*c)

raizes a b c = nRaizes a b c

-------------------------

type Hora = (Int,Int)
valida :: Hora -> Bool
valida (h,m) = h<=23 && h>=0 && m<=59 && m>=0

depois (h1, m1) (h2, m2) = 
    if h2>h1 || (h1 == h2 && m2>m1)
    then True
    else False

hora2minute :: Hora -> Int
hora2minute (h,m) = h*60 +m

difhora :: Hora ->Hora ->Int
difhora a b = abs(hora2minute(a) - hora2minute b)

--------construir novos tipos de daodos -> data X = x || y
data Hora2 = H Int Int deriving(Show, Eq)
valida2 :: Hora2 -> Bool
valida2 (H h m) = valida(h,m)

data Semaforo = Verde | Amarelo | Vermelho deriving(Show,Eq)
next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

stop :: Semaforo -> Bool
stop Vermelho = True
stop Verde = False
stop Amarelo = False

safe :: Semaforo -> Semaforo ->Bool
safe x y =
    if stop x || stop y == True
    then False
    else True

data Ponto = Cartesiano Double Double | Polar Double Double deriving(Show,Eq)
--Transformar em Polar
converterp (Polar a b) = do
    let z = dist2(0,0)(a,b)
    let angle_radian = atan(b/a)
    let angle_degrees = angle_radian * 57.3
    (z,angle_degrees)

--Transformar em Cartesiano
converterc (Cartesiano a b)= do
    let z = a
    let x = z * (cos(b*(pi/180)))
    let y = z * (sin(b*(pi/180)))
    (x,y)

--6
--a)
posx :: Ponto-> Double
posx (Cartesiano a b) = a 

posx2 :: Char -> Double -> Double -> Double
posx2 a b c = do
    let z = b
    let y = c
    let x = z * (cos(y*(pi/180)))
    if a == 'C' || a == 'c'
        then c
        else if a == 'P' || a == 'p'
             then x
             else x
--b)
posy :: Ponto -> Double
posy(Cartesiano a b) = b

posy(Polar a b)=do
    let z = a
    let y = z * (sin(b*(pi/180)))
    y

posy2 :: Char -> Double -> Double -> Double
posy2 a b c = do
    let z = b
    let y = z * (sin(c*(pi/180)))
    if a == 'C' || a == 'c'
    then c
    else if a == 'P' || a == 'p'
         then y
         else y
--c)
raio::Ponto ->Double
raio(Cartesiano a b)= dist2(0,0)(a,b)
raio (Polar a b) = do
    let z = a
    let x = z * (cos(b*(pi/180)))
    let y = z * (sin(b*(pi/180)))
    dist2(0,0)(x,y)    

raio2::Char -> Double -> Double -> Double
raio2 a b c = do
    let z = b
    let x = z * (cos(c*(pi/180)))
    let y = z * (sin(c*(pi/180)))  
    if a == 'C' || a == 'c'
    then dist2(0,0)(b,c)
    else if a == 'P' || a == 'p'
         then dist2(0,0)(x,y)
         else dist2(0,0)(x,y)

angulo :: Ponto -> Double
angulo(Cartesiano a b)= atan((b/a))*(180/pi)
angulo (Polar b c) = c


dist' :: Ponto -> Ponto -> Double
dist' (Cartesiano a b)(Cartesiano c d) = dist2(a,b)(c,d)
dist' (Cartesiano a b)(Polar c d) = do
    let z = c
    let x = z * (cos(d*(pi/180)))
    let y = z * (sin(d*(pi/180)))
    dist2(a,b)(x,y)

dist' (Polar a b)(Cartesiano c d) = do
    let z = a
    let x = z * (cos(b*(pi/180)))
    let y = z * (sin(b*(pi/180)))
    dist2(c,d)(x,y)   

dist' (Polar a b)(Polar c d) = do
    let z = a
    let x = z * (cos(b*(pi/180)))
    let y = z * (sin(b*(pi/180)))
    let o = c * (cos(d*(pi/180)))
    let p = z * (sin(d*(pi/180)))
    dist2(x,y)(o,p)   

data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving(Show, Eq)

poligono::Figura -> Bool
poligono (Circulo a b)= False
poligono (Rectangulo a b)= True
poligono (Triangulo a b c) = True

-- E se os pontos não tiverem todos o mesmo tipo de input?
vertices :: Figura -> [Ponto]
vertices(Triangulo a b c) = [a,b,c]

{-
area :: Figura -> Double
area (Triangulo p1 p2 p3) = 
    let a = distCart p1 p2
        b = distCart p2 p3
        c = distCart p3 p1
        s = (a +b +c) / 2 -- semi perimetro
    in sqrt ((s*(s-a)*(s-b)*(s-c))) --formula de Heron
-}


--8
isLower' :: Char -> Bool
isLower' a = do
    let b = ['a'..'z']
    a `elem` b 

isLower2' :: Char -> Bool
isLower2' a = do
    let b = [97..122]
    ord (a) `elem` b


isdigit :: Char -> Bool
isdigit a = do
    let b = [48..57]
    ord a `elem` b


isalpha :: Char -> Bool 
isalpha a = do
    let b = [65..90]
    let c = [97..122]
    ord a `elem` b || ord a `elem`c
    
upperto' :: Char -> Char
upperto' a = do
    let b  = isLower' a
    let c = ord a
    if b == True
        then chr(c-32)
        else chr(c)

int2digit :: Int -> Char
int2digit a = chr(48+a)

digit2int :: Char -> Int
digit2int a = do
    let b = ord a
    let c = b-48
    c
        