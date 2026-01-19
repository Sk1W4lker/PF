module Ficha1 where
import Data.Char

{-
> Funções não recursivas

•length l: o numero de elementos da lista l
•head l: a cabeca da lista (nao vazia) l
•tail l: a cauda lista (nao vazia) l
•last l: o  ́ultimo elemento da lista (nao vazia) l
•sqrt x: a raiz quadrada de x
•div x y: a divisao inteira de x por y
•mod x y: o resto da divisao inteira de x por y
-}

--1)
--a) calcula o per ́ımetro de uma circunferˆencia, dado o comprimento do seu raio.
perimetro :: Double -> Double
perimetro l = 2*pi*l

--b) calcula a distˆancia entre dois pontos no plano Cartesiano
dist :: (Double, Double) -> (Double, Double) -> Double
dist (x,y) (x2,y2) = sqrt((x-x2)^2 + (y-y2)^2)

--c) recebe uma lista e devolve um par com o primeiro e o  ́ultimo
primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)

--d) testa se é multiplo
multiplo :: Int -> Int -> Bool
multiplo x y = mod x y == 0

--e)  recebe uma lista e, se o comprimento da lista for  ́ımpar retira-lhe o primeiro elemento, caso contr ́ario devolve a pr ́opria lista.
truncaImpar :: [a] -> [a]
truncaImpar l = if mod (length l) 2 == 0 then l
            else tail l

--f) calcula o maior de dois n ́umeros inteiros
max2 :: Int -> Int -> Int
max2 x y = if x > y then x else y

--g) calcula o maior de trˆes n ́umeros inteiros, usando a fun ̧c ̃ao max2
max3 :: Int -> Int -> Int -> Int
max3 a b c = max2(max2 a b) c

--2
--a)  recebe os (3) coeficientes de um polin omio de 2o grau e que calcula o n ́umero de ra ıze
nRaizes :: (Double, Double, Double) -> Int 
nRaizes (a,b,c) | b^2 - 4*a*c == 0 = 1
                | b^2 - 4*a*c > 0 = 2
                | b^2 - 4*a*c < 0 = 0
                | otherwise = 0

--b)  recebe os coeficientes do polin ́omio e calcula a lista das suas ra ızes reais
raizes :: (Double, Double, Double) -> [Double]
raizes (a,b,c)  | nRaizes (a,b,c) == 2 = [(-b + sqrt(b ^ 2 - 4 * a * c))/(2*a), (-b-sqrt(b ^ 2 - 4 * a * c))/(2*a)]
                | nRaizes (a,b,c) == 1 = [-b / (2*a)]
                | nRaizes (a,b,c) == 0 = []
                where n = nRaizes(a,b,c)

--3)

type Hora = (Int,Int)

--a) testar se um par de inteiros representa uma hora do dia v ́alida;
validaHora :: Hora -> Bool
validaHora (h,m) = h >= 0 && h <= 23 && m >= 0 && m <= 59

--b) testar se uma hora  ́e ou n ̃ao depois de outra (compara ̧c ̃ao);
horaDepois :: Hora -> Hora -> Bool
horaDepois (h,m) (h2,m2) = h == h2 && m > m2 || h > h2

--c) converter um valor em horas (par de inteiros) para minutos (inteiro);
converteHoras :: Hora -> Int
converteHoras (h,m) = h*60+m

--d) converter um valor em minutos para horas;
converteMinutosHoras :: Int -> Hora
converteMinutosHoras l = (div l 60, mod l 60)

--e) calcular a diferenca entre duas horas (cujo resultado deve ser o numero de minutos)
diferencaHoras :: Hora -> Hora -> Int
diferencaHoras (h,m) (h2,m2) = (h-h2)*60 + (m-m2)

--f) adicionar um determinado numero de minutos a uma dada hora
adicionaMinutos :: Hora -> Int -> Hora
adicionaMinutos (h,m) n = (h+ hr,mr)
    where (hr,mr) = converteMinutosHoras (m+n)

--4) Repita o exercicio anterior usando data HoraH

data HoraH = H Int Int deriving (Show,Eq)

--a)
horaValida1 :: HoraH -> Bool
horaValida1 (H a b) = a >= 0 && a<=23 && b >=0 && b<=59

--b)
horaDepoisDe1 :: HoraH -> HoraH -> Bool
horaDepoisDe1 (H h1 m1) (H h2 m2) = h1 > h2 || (h1 == h2 && m1 > m2)

--c)
horaParaMinutos1 :: HoraH -> Int
horaParaMinutos1 (H h m) = 60 * h + m

--d)
minutosParaHora1 :: Int -> HoraH
minutosParaHora1 m = (H (div m 60) (mod m 60)) 

--e)
diferencaHoras1 :: HoraH -> HoraH -> Int
diferencaHoras1 (H h1 m1) (H h2 m2) = (h1 - h2) * 60 + (m1 - m2)

--f)
adicionaMinutos1 :: HoraH -> Int -> HoraH
adicionaMinutos1 (H h m) n = minutosParaHora1 (horaParaMinutos1 (H h m) + n)

adicionaMinutos1' :: HoraH -> Int -> HoraH
adicionaMinutos1' (H h m) n = H (h + hr) mr
    where (H hr mr) = minutosParaHora1 (m + n)

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

--a) calcula o pr ́oximo estado de um sem ́aforo
next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

--b) determina se é obrigatorio parar num sem ́aforo.
stop :: Semaforo -> Bool
stop Vermelho = True
stop _ = False

--c) testa se o estado de dois sem ́aforos num cruzamento ́e seguro
safe :: Semaforo -> Semaforo -> Bool
safe Vermelho _ = True
safe _ Vermelho = True
safe _ _ = False

--6)
data Ponto = Cartesiano Double Double | Polar Double Double
            deriving (Show,Eq)

--a) calcula a distˆancia de um ponto ao eixo vertical.
posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = r * cos a

--b) calcula a distˆancia de um ponto ao eixo horizontal.
posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar r a) = r * sin a

--c) calcula a distˆancia de um ponto `a origem.
raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x^2 + y^2)
raio (Polar r a) = r

--d) calcula o ˆangulo entre o vector que liga a origem a um ponto e o eixo horizontal.
angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (y/x)
angulo (Polar r a) = a

--e) calcula a distˆancia entre dois pontos
dist1 :: Ponto -> Ponto -> Double
dist1 p1 p2 = sqrt ((x1 - x)^2 + (y1-y)^2)
    where x = posx p1
          y = posy p1
          x1 = posx p2
          y1 = posy p2

--7)
data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show,Eq)

--a) testa se uma figura  ́e um pol ́ıgono
poligono :: Figura -> Bool
poligono (Circulo _ _) = False --circulos não são poligonos
poligono (Retangulo p1 p2) = (posx p1 /= posx p2) && (posy p1 /= posy p2)
poligono (Triangulo p1 p2 p3) =  (posx p1 /= posx p2 || posx p2 /= posx p3 || posx p1 /= posx p3) &&
  (posy p1 /= posy p2 || posy p2 /= posy p3 || posy p1 /= posy p3)
--b) calcula a lista dos vertices de uma figura.
vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Retangulo p1 p2) = [p1, Cartesiano (posx p1) (posy p2), p2, Cartesiano (posx p2) (posy p1)]
vertices (Triangulo p1 p2 p3) = [p1, p2, p3]

--c) area de uma figura
area :: Figura -> Double
area (Circulo _ r) = pi*r^2
area (Retangulo p1 p2) = abs (posx p2 - posx p1) * abs (posy p2 - posy p1)
area (Triangulo p1 p2 p3) = let a = dist1 p1 p2
                                b = dist1 p2 p3
                                c = dist1 p3 p1
                                s = (a+b+c) / 2 -- semi-perimetro
                                in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron

--d) calcula o perımetro de uma figura.
perimetro1 :: Figura -> Double
perimetro1 (Circulo _ r) = 2*pi*r
perimetro1 (Retangulo p1 p2) = abs(posx p2 - posx p1) * 2 + abs(posy p2 - posy p1) * 2
perimetro1 (Triangulo p1 p2 p3) = dist1 p1 p2 + dist1 p2 p3 + dist1 p1 p3

--8) 

--a) testa se um Char  ́e uma minuscula.
isLower1 :: Char -> Bool
isLower1 a = ord a >= ord 'a' && ord a <= ord 'z'

--b) verifica se é digito
isDigit1 :: Char -> Bool
isDigit1 a = ord a >= ord '0' && ord a <= ord '9'

--c) testa se um Char  ́e uma letra
isAlpha :: Char -> Bool
isAlpha ch = (ord ch >= ord 'a' && ord ch <= ord 'z') || (ord ch >= ord 'A' && ord ch <= ord 'Z')

--d) converte uma letra para a respectiva mai ́uscula
toUpper1 :: Char -> Char
toUpper1 ch = if isLower1 ch then chr ((ord ch) - 32) else ch

--e) converte um n ́umero entre 0 e 9 para o respectivo digito.
intToDigit1 :: Int -> Char
intToDigit1 a = chr (a + 48)

--f) converte um digito para o respectivo inteiro.
digitToInt1 :: Char -> Int
digitToInt1 a = ord (a) - 48