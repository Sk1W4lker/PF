module Ficha8 where
import Data.List
import Data.Char

data Frac = F Integer Integer

{-
(a) Defina a fun ̧c ̃ao normaliza :: Frac -> Frac, que dada uma frac ̧c ̃ao calcula uma
frac ̧c ̃ao equivalente, irredut ́ıvel, e com o denominador positivo. Por exemplo,
normaliza (F (-33) (-51)) deve retornar F 11 17 e normaliza (F 50 (-5))
deve retornar F (-10) 1. Sugere-se que comece por definir primeiro a fun ̧c ̃ao
mdc :: Integer -> Integer -> Integer que calcula o m ́aximo divisor comum
entre dois n ́umeros, baseada na seguinte propriedade (atribuida a Euclides):
mdc x y == mdc (x+y) y == mdc x (y+x)
-}
normaliza :: Frac -> Frac
normaliza (F x y) = F (s * a) b
  where
    d = mdc (abs x) (abs y)   -- mdc dos valores absolutos
    a = div (abs x) d         -- numerador simplificado
    b = div (abs y) d         -- denominador simplificado
    s = signum (x * y)        -- sinal final da fracção Signum retorna -1 se for negativa o numero, e 1 se for positivo
{-
Exemplo (F (-6) 8)
d = mdc 6 8 = 2
a = div 6 2 = 3
b = div 8 2 = 4
s = signum ((-6)*8) = -1

normaliza(F (-6) 8) = F ((-1) * 3) 4 R: F (-3) 4


-}
mdc :: Integer -> Integer -> Integer 
mdc x 0 = x --Se o segundo número é 0 acabou e devolve
mdc 0 y = y 
mdc x y = mdc y (mod x y) --Até (mod x y) ser 0 e y ser o maior divisor

mdc' :: Integer -> Integer -> Integer
mdc' a b | a == b = a
         | a < b = mdc a (b-a)
         | a > b = mdc (a-b) b

-- b) Defina Frac como instância da classe Eq.
instance Eq Frac where
    (==) = fracEq

fracEq :: Frac -> Frac -> Bool
fracEq f f' = x == x2 && y == y2 
    where (F x y) = normaliza f
          (F x2 y2) = normaliza f'

--(c) Defina Frac como instˆancia da classe Ord.
instance Ord Frac where
    (<=) = fracOrd

fracOrd :: Frac -> Frac -> Bool
fracOrd f f' = x*y' <= x'*y --Queremos saber x/y <= x'/y' e não posso fazer x <= x' e y <= y', pois não funciona então usamos produto cruzado a*d <= c*b
    where (F x y) = normaliza f
          (F x' y') = normaliza f'

--(d) Defina Frac como instˆancia da classe Show, de forma a que cada frac ̧c ̃ao seja apresentada por (numerador/denominador)

instance Show Frac where
    show = fracShow

fracShow :: Frac -> String
fracShow f = show x ++ "/" ++ show y
        where (F x y) = normaliza f

{-
(e) Defina Frac como instˆancia da classe Num. Relembre que a classe Num tem a seguinte defini ̧c ̃ao
class (Eq a, Show a) => Num a where
(+), (*), (-) :: a -> a -> a
negate, abs, signum :: a -> a
fromInteger :: Integer -> a
-}

instance Num Frac where
    (+) = somaFrac
    (*) = multFrac
    (-) = subtFrac
    negate = negFrac
    abs = absFrac
    signum = signumFrac
    fromInteger = fromIntegerFrac

somaFrac :: Frac -> Frac -> Frac
somaFrac f f' = (F (x*y' + x'*y) (y*y'))
    where (F x y) = normaliza f
          (F x' y') = normaliza f'

multFrac :: Frac -> Frac -> Frac
multFrac f f' = (F (x*x') (y*y'))
    where (F x y) = normaliza f
          (F x' y') = normaliza f'

subtFrac :: Frac -> Frac -> Frac
subtFrac f f' = (F (x*y' + x'*y) (y*y'))
    where (F x y) = normaliza f
          (F x' y') = normaliza f'

negFrac :: Frac -> Frac
negFrac f = (F (negate x) y)
    where (F x y) = normaliza f

absFrac :: Frac -> Frac
absFrac f = (F (abs x) (abs y))
    where (F x y) = normaliza f

signumFrac :: Frac -> Frac
signumFrac f = (F (signum a) (signum b))
        where F a b = normaliza f

fromIntegerFrac :: Integer -> Frac 
fromIntegerFrac x = F x 1 

{-
f) Defina uma fun ̧c ̃ao que, dada uma frac ̧c ̃ao f e uma lista de frac ̧c ̃oes l, selecciona
de l os elementos que s ̃ao maiores do que o dobro de f.
-}
seleciona :: Frac -> [Frac] -> [Frac] 
seleciona f = filter (>dobroF) 
    where dobroF = 2*f 
{-
2. Relembre o tipo definido na Ficha 7 para representar express ̃oes inteiras. Uma poss ́ıvel
generaliza ̧c ̃ao desse tipo de dados, ser ́a considerar express ̃oes cujas constantes s ̃ao de
um qualquer tipo num ́erico (i.e., da classe Num).
-}
data Exp a = Const a
    | Simetrico (Exp a)
    | Mais (Exp a) (Exp a)
    | Menos (Exp a) (Exp a)
    | Mult (Exp a) (Exp a)

--(a) Declare Exp a como uma instˆancia de Show.
instance (Show a) => Show (Exp a) where
    show = showEsp

showEsp :: Show a => Exp a -> String
showEsp (Const a) = show a --Show a => Exp a permite usar show, se fosse só Show (Exp a) não
showEsp (Simetrico a) = "(-" ++ showEsp a ++ ")"
showEsp (Mais x y) = "(" ++ showEsp x ++ " + " ++ showEsp y ++ ")"
showEsp (Menos x y) = "(" ++ showEsp x ++ " - " ++ showEsp y ++ ")"
showEsp (Mult x y) = "(" ++ showEsp x ++ " x " ++ showEsp y ++ ")"

--b) Declare Exp a como uma instˆancia de Eq
valorDe :: Num a => Exp a -> a
valorDe (Const a) = a
valorDe (Simetrico a) = -(valorDe a)
valorDe (Mais x y) = valorDe x + valorDe y
valorDe (Menos x y) = valorDe x - valorDe y
valorDe (Mult x y) = valorDe x * valorDe y

expEq :: (Eq a, Num a) => Exp a -> Exp a -> Bool
expEq f f' = valorDe f == valorDe f'

instance (Num a, Eq a) => Eq (Exp a) where
    (==) = expEq

--(c) Declare Exp a como instˆancia da classe Num

somaExp :: Num a => Exp a -> Exp a -> Exp a
somaExp f f' = (Mais f f')

multExp :: Num a => Exp a -> Exp a -> Exp a
multExp f f' = (Mult f f')

subtExp :: Num a => Exp a -> Exp a -> Exp a
subtExp f f' = (Menos f f')

negateExp :: Num a => Exp a -> Exp a
negateExp f = (Simetrico f)

absExp :: Num a => Exp a -> Exp a
absExp f = Const (abs x)
        where x = valorDe f

signumExp :: Num a => Exp a -> Exp a
signumExp f = Const (signum x) --signumExp exp = Const $ signum (calcula exp)
           where x = valorDe f

fromIntegerExp :: Num a => Integer -> Exp a
fromIntegerExp f = Const (fromInteger f)

instance Num a => Num (Exp a) where
    (+) = somaExp
    (-) = subtExp
    (*) = multExp
    negate = negateExp
    abs = absExp
    signum = signumExp
    fromInteger = fromIntegerExp

--3. Relembre o exercıcio da Ficha 3 sobre contas bancarias, com a seguinte declaração de tipos

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extrato = Ext Float [(Data, String, Movimento)]

ext :: Extrato 
ext = Ext 500 [(D 4 5 2001,"Deposito",Credito 300),
               (D 5 9 2003,"Propinas",Debito 100),
               (D 27 11 1999,"Financas",Debito 150),
               (D 13 3 2002,"Emprego",Credito 200)]

--Extra para não dar erro
dataEq :: Data -> Data -> Bool
dataEq (D d1 m1 a1) (D d2 m2 a2) = a2 == a1 && m2 == m1 && d2 == d1

instance Eq Data where
    (==) = dataEq

--(a) Defina Data como instancia da classe Ord
dataOrd :: Data -> Data -> Bool
dataOrd (D d1 m1 a1) (D d2 m2 a2) = a2 > a1 || a2 == a1 && m2 > m1 || a2 == a1 && m2 == m1 && d2 > d1

instance Ord Data where
    (<=) = dataOrd

--(b) Defina Data como instˆancia da classe Show.
dataShow :: Data -> String
dataShow (D d m a) = show d ++ "/" ++ show m ++ "/" ++ show a

instance Show Data where
    show = dataShow
 
 --c)  Defina a fun ̧c ̃ao ordena :: Extracto -> Extracto, que transforma um extracto de modo a que a lista de movimentos apare ̧ca ordenada por ordem crescente de data.
ordena :: Extrato -> Extrato 
ordena (Ext x l) = Ext x listOrdenada
    where listOrdenada = sortOn fstTriple l --Ordena a lista de triplos pelo primeiro elemento.
{-
l = [(3,"a",1), (1,"b",2), (2,"c",3)]

sortOn fstTriple l
→ [(1,"b",2), (2,"c",3), (3,"a",1)]
-}
-- Função que retorna o primeiro elemento de um triplo 
fstTriple :: (a,b,c) -> a 
fstTriple (x,y,z) = x 

{-
d) Defina Extracto como instˆancia da classe Show, de forma a que a apresenta ̧c ̃ao do
extracto seja por ordem de data do movimento com o seguinte, e com o seguinte
aspecto
-}                               
showExtrato :: Extrato -> String 
showExtrato extrato = let (Ext ini list) = ordena extrato 
                      in "-----------------------------------------\n" ++ 
                         "Saldo Inicial: " ++ show ini ++ "\n" ++
                         "-----------------------------------------\n" ++ 
                         "Data        Descrição    Crédito   Débito\n" ++
                         "-----------------------------------------" ++ "\n" ++ 
                         concat (map showMovs list) ++ 
                         "-----------------------------------------" ++ "\n" ++ 
                         "Saldo Atual: " ++ show (saldoAtual extrato) ++ "\n" ++ 
                         "-----------------------------------------"

showMovs :: (Data,String,Movimento) -> String 
showMovs (date,desc,Credito x) = 
    show date ++ 
    (replicate (data_max - length (show date)) ' ') ++ 
    (map toUpper desc) ++ 
    (replicate (desc_max - length desc) ' ') ++ 
    show x ++ "\n"
    where data_max = 12
          desc_max = 13
showMovs (date,desc,Debito x) = 
    show date ++ 
    (replicate (data_max - length (show date)) ' ') ++ 
    (map toUpper desc) ++ 
    (replicate (desc_max - length desc) ' ') ++ 
    replicate cred_max ' ' ++ 
    show x ++ "\n"
    where data_max = 12
          desc_max = 12
          cred_max = 11

saldoAtual :: Extrato -> Float
saldoAtual (Ext ini list) = ini + sum listMovs 
    where listMovs = map (\ (_,_,mov) -> case mov of 
                                              Credito x -> x
                                              Debito y -> (-y)) list 

instance Show Extrato where 
    show = showExtrato 