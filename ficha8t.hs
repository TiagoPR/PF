import Data.List

-- Exercicio 1

data Frac = F Integer Integer -- (deriving show) para corrigir o erro abaixo

-- a)

normaliza :: Frac -> Frac
normaliza (F x y) = F (div x m) (div y m)
    where
        m = (mdc x y)

mdc x 0 = x
mdc x y = mdc y (x `mod` y)

-- <interactive>:6:1: error:
--    * No instance for (Show Frac) arising from a use of `print'
--    * In a stmt of an interactive GHCi command: print it

-- b)

-- pode correr mal se a multiplicação for um Integer muito grande

instance Eq Frac where                                                                      
    (F x y) == (F x1 y1) = (x * y1) == (y * x1)

    --                 ou (normaliza(F x y)) == (normaliza (F x1 y1))

-- c)

instance Ord Frac where 
    compare (F x1 y1) (F x2 y2)
            | c1 < c2 = LT 
            | c1 == c2 = EQ  -- \ (F x1 y1) == (F x2 y2) = EQ
            | c1 > c2 = GT
        where
            c1 = (fromIntegral x1) / (fromIntegral y1)
            c2 = (fromIntegral x2) / (fromIntegral y2)

-- d)

instance Show Frac where
    show (F x y) = show x ++ "/" ++ show y

-- e)

instance Num Frac where
    (+) = somaFrac
    (*) = multFrac
    negate (F x y) = (F (-x) y)
    abs (F x y) = (F (abs x) (abs y))
    signum (F x y) = normaliza (F (signum x) (signum y)) -- o signum já esta definido no prelude
                -- ou (F ((signum x) * (signum y)) 1)
    fromInteger x = F x 1

somaFrac :: Frac -> Frac -> Frac
somaFrac (F x1 y1) (F x2 y2) = normaliza (F (x1 * y2 + x2*y1) (y1 * y2))

multFrac :: Frac -> Frac -> Frac
multFrac (F x1 y1) (F x2 y2) = normaliza (F (x1*x2) (y1*y2))

-- f)

selFrac :: Frac -> [Frac] -> [Frac]
selFrac f l = filter (\x -> f * (fromInteger 2) < x) l  -- tem de ter o fromInteger pq definimos que (*) é a multiplicacao de Fracs


-- Exercicio 2

data Exp a = Const a
    | Simetrico (Exp a)
    | Mais (Exp a) (Exp a)
    | Menos (Exp a) (Exp a)
    | Mult (Exp a) (Exp a)

-- a)

instance Show a => Show (Exp a) where                                    -- temos de buscar a instancia de Show a pq nao sabemos o tipo de a
    show (Const a) = show a                                              -- Enquanto que nos exs acima sabiamos que era Integer
    show (Simetrico a) = "(- " ++ show a ++ ")"
    show (Mais a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show (Menos a b) = "(" ++ show a ++ "-" ++ show b ++ ")"
    show (Mult a b) = "(" ++ show a ++ "*" ++ show b ++ ")"


-- b)

instance (Eq a, Num a) => Eq (Exp a) where      -- Temos de instanciar o Num a pq usamos a funcao calcula que precisa da classe Num
    x == y = (calcula x) == (calcula y)

-- c)

calcula :: Num a => Exp a -> a 
calcula expr = case expr of
    Const n -> n                -- Se for uma constante só dá ela propria visto que as constantes sõ são folhas
    Mais e1 e2 -> (calcula e1) + (calcula e2) -- e1 e e2 são do tipo ExpInt
    Menos e1 e2 -> (calcula e1) - (calcula e2) 
    Mult e1 e2 -> (calcula e1) * (calcula e2)

instance Num a => Num (Exp a) where
    (+) c1 c2 = Mais c1 c2
    (*) c1 c2 = Mult c1 c2
    (-) c1 c2 = Menos c1 c2
    signum c = Const (signum (calcula c))
    abs c = c * signum c
    fromInteger n = Const (fromInteger n)

-- Exercicio 3

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int deriving Eq    -- na alinea a) estava com erro pq nao tinha instancia de Eq , logo temos de fazer (deriving Eq)
data Extracto = Ext Float [(Data, String, Movimento)]

-- a)

instance Ord Data where
    compare (D a m d) (D a2 m2 d2)
        | (a >= a2) && (m >= m2) && (d > d2) = GT
        | (a == a2) && (m == m2) && (d == d2) = EQ
        | otherwise = LT

-- b)

instance Show Data where
    show (D a m d) = show a ++ "/" ++ show m ++ "/" ++ show d
    -- show (D dia mes ano) = concat $ intersperse "/" $ map (show) [dia,mes,ano]    * tentar ver como funciona

-- c)
-- (Ext 100 [((D 2020 12 22),"Compras 2020",Credito 20),((D 2019 12 22),"Compras 2019",Credito 20)])
ordena :: Extracto -> Extracto
ordena (Ext n l) = (Ext n (sortBy (\(data1,_,_) (data2,_,_) -> compare data1 data2) l))