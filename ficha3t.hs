
data Hora = H Int Int deriving Show
type Etapa = (Hora,Hora)
type Viagem = [Etapa]


min2hora :: Int -> Hora          ---- Falta-me ter os minutos
min2hora m = (H (m `div` 60) 0)

hora2min :: Hora -> Int
hora2min (H h m) = h*60+m

difhora :: Hora -> Hora -> Int
difhora h1 h2 = abs(min1 - min2)
    where
        min1 = hora2min h1
        min2 = hora2min h2

depois :: Hora -> Hora -> Bool
depois (H h1 m1) (H h2 m2) =
    if(h1>h2) then True
    else if (h1 == h2) && m1>m2 then True
    else False

addMin :: Int -> Hora -> Hora
addMin m (H h m2) = if (n >= 60) then (H (h+h1) m3)
        else (H h (n))
    where n = m2 + m
          h1 = div (m+m2) 60 
          m3 = mod (m+m2) 60 


-- Exercicio 1 

-- a)

etapaValida :: Etapa -> Bool
etapaValida (p,c) = (depois c p)

-- b)

viagemValida :: Viagem -> Bool
viagemValida [] = True
viagemValida [e] = (etapaValida e)
viagemValida ((c,f):(c2,f2):es) = etapaValida (c,f) && etapaValida (f,c2) && viagemValida((c2,f2):es)

-- c)

partidaChegada :: Viagem -> Etapa
partidaChegada [] = error "Viagem inválida"
partidaChegada [e] = e
partidaChegada v = (fst(head v),snd(last v))

-- d)

tempoEfetivo :: Viagem -> Hora
tempoEfetivo [] = H 0 0
-- tempoEfetivo ((c,f):es) = (addMin (difhora c f)             // FALTA SEGUNDO PARAMETRO DO ADDMIN

-- e)

tempoEspera :: Viagem -> Hora
tempoEspera ((c,f):(c2,f2):es) = (addMin (difhora f c2) (tempoEspera ((c2,f2):es)))
tempoEspera _ = (H 0 0) 


-- f) 

tempoTotal :: Viagem -> Hora
tempoTotal v = min2hora (difhora c f)
    where (c,f) = partidaChegada v

-- alternativa

-- tempoTotal' :: Viagem -> Hora
-- tempoTotal' v = (addMin (tempoEspera v) (hora2min(tempoEfetivo v)))




-- Exercicio 2

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

type Poligonal = [Ponto]

dist :: (Double , Double) -> (Double , Double) -> Double
dist (x1, y1) (x2, y2) = sqrt((((x1-x2)^2)+ (y1-y2)^2))

cartesiano2polar :: Ponto -> Ponto
cartesiano2polar (Cartesiano x y) = (Polar (sqrt(x^2 + y^2)) (atan (y/x)))
cartesiano2polar p = p -- Os outros casos devolvem o próprio ponto visto que é Polar already.

polar2cartesiano (Polar r a) = (Cartesiano (r*cos a)  (r*sin a))
polar2cartesiano p = p

dist2 p1 p2 = sqrt((x-a)^2 + (y-b)^2) where
    (Cartesiano x y) = polar2cartesiano p1
    (Cartesiano a b) = polar2cartesiano p2

posx :: Ponto -> Double 
posx (Cartesiano x y) = x
posx (Polar r a) = r * cos a

posy (Cartesiano x y) = y
posy (Polar r a) = r * sin a

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




-- a)

cumprimento :: Poligonal -> Double
cumprimento (p0:p1:ps) = (dist2 p0 p1)
cumprimento _ = 0.0

-- b)

fechada :: Poligonal -> Bool
fechada l = (length l >= 4) && cartesiano2polar (head l) == cartesiano2polar (last l)

-- c)

triangula :: Poligonal -> [Figura]
triangula (a:b:c:d:t) = (Triangulo a b c) : triangula (a:c:d:t)
triangula _ = []

-- d)

areaPoligonal :: Poligonal -> Double
areaPoligonal l = (somaArea (triangula l))

--aux

somaArea :: [Figura] -> Double
somaArea [] = 0.0
somaArea (h:t) = area h + (somaArea t)

-- e)

mover :: Poligonal -> Ponto -> Poligonal
mover [] _ = []
mover (h:t) p0 = mover' (h:t) a b
    where 
        (Cartesiano x y) = (polar2cartesiano h)
        (Cartesiano x' y') = (polar2cartesiano p0)
        (a,b) = (x-x' , y-y')

mover' :: Poligonal -> Double -> Double -> Poligonal
mover' [] _ _ = []
mover' (h:t) a b = (Cartesiano (x-a) (y-b)) : (mover' t a b)
    where
        (Cartesiano x y) = (polar2cartesiano h)

-- f)

zoom :: Double -> Poligonal -> Poligonal
zoom _ [] = []
zoom a [p] = [p]
zoom a (p1:p2:t) = p1 : p2' : zoom a (mover (p2:t) p2')
    where
        (Cartesiano x y) = polar2cartesiano p1
        (Cartesiano x' y') = polar2cartesiano p2
        nx = (x' - x) * a + x
        ny = (y' - y) * a + y 
        p2' = Cartesiano nx ny


-- Exercicio 3

data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

-- a)

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n e [] = [(n, [Email e])]
acrescEmail n e ((n',c):cs)
    | n == n' = (n', addEmail e c) : cs
    | otherwise = (n',e) : (acrescEmail n e cs)

-- aux

addEmail :: String -> [Contacto] -> [Contacto]
addEmail e [] = [Email e]
addEmail e ((Email e'):cs)
    | e == e' = ((Email e'):cs)
    | otherwise = ((Email e'):cs)
addEmail e (c:cs) = c : (addEmail e cs)
