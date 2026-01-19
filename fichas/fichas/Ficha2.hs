module Ficha2 where
import Data.Char

--Funçoes recursivas sobre listas

--a)
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)
{-
Diga, justificando, qual  ́e o valor de funA [2,3,5,1].

R: 2^2 + 3^2 + 5^2 + 1^2 = 39
-}

--b)
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)==0 then h : (funB t)
else (funB t)
{-
Diga, justificando, qual  ́e o valor de funB [8,5,12]

R: [8,12]
-}

--c)
funC (x:y:t) = funC t
funC [x] = []
funC [] = []

{-
Diga, justificando, qual  ́e o valor de funC [1,2,3,4,5].

R: funC [1,2,3,4,5] = funC (1:2:[3,4,5])
                    = funC (3:4:[5])
                    = funC [5]
                    = [5]
-}

funD l = g [] l
g l [] = l
g l (h:t) = g (h:l) t
{-
Diga, justificando, qual  ́e o valor de funD "otrec".

R: funD "otrec"
    = g [] "otrec"
    = g [] ('o':"trec")
    = g ('o':[]) "trec"
    = g "o" ('t':"rec")
    = g ('t':"o") "rec"
    = g "to" ('r':"ec")
    = g ('r':"to") "ec"
    = g "rto" ('e':"c")
    = g ('e':"rto") "c"
    = g "erto" ('c':[])
    = g ('c':"erto") []
    = "certo"
-}

--2)
--a) que recebe uma lista e produz a lista em que cada elemento  ́e o dobro do valor correspondente na lista de entrada.
dobros :: [Float] -> [Float] 
dobros [] = []
dobros (h:t) = h*2 : dobros t

--b) que calcula o numero de vezes que um caracter ocorre numa string
numOcorre :: Char -> String -> Int
numOcorre _ "" = 0
numOcorre c (h:t) | c == h = 1 + numOcorre c t
                  | otherwise = numOcorre c t

--c)  que testa se uma lista s ́o tem elementos positivos
positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) | h <= 0 = False
                | otherwise = positivos t

--d) ue retira todos os elementos n ̃ao positivos de uma lista de inteiros.
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) | h <= 0 = soPos t
            | otherwise = h : soPos t

--e) soma todos os n ́umeros negativos da lista de entrada.
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) | h <= 0 = h + somaNeg t
              | otherwise = somaNeg t

--f) devolve os  ́ultimos trˆes elementos de uma lista. Se a lista de entrada tiver menos de trˆes elementos, devolve a pr ́opria lista. 1
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t) | length t < 3 =  (h:t)
              | otherwise = tresUlt t

--g) calcula a lista das segundas componentes dos pares.
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((h,t):t1) = t : segundos t1 

--h)
-- testa se um elemento aparece na lista como primeira componente de algum dos pares.
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros a [] = False
nosPrimeiros a ((h,t):t1) | a == h = True
                          | otherwise = nosPrimeiros a t1

--i) 
--soma uma lista de triplos componente a componente.
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0, 0, 0)
sumTriplos ((x,y,z):t) = (x+x1,y+y1,z+z1)
    where (x1,y1,z1) = sumTriplos t

--3)
--a)que recebe uma lista de caracteres, e selecciona dessa lista os caracteres que s ̃ao algarismos.
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) | isDigit h = h : soDigitos t
                | otherwise = soDigitos t

--b) que recebe uma lista de caracteres, e conta quantos desses caracteres são letras minusculas.
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) | isLower h = 1 + minusculas t
                 | otherwise = minusculas t

--c)que recebe uma string e devolve uma lista com os algarismos que ocorrem nessa string, pela mesma ordem.
nums :: String -> [Int]
nums "" = []
nums (h:t) | isDigit h = digitToInt h : nums t
           | otherwise = nums t

{-
4. Uma forma de representar polinomios de uma vari ́avel  ́e usar listas de monomios rep-
resentados por pares (coeficiente, expoente)
type Polinomio = [Monomio]
type Monomio = (Float,Int)
Por exemplo, [(2,3), (3,4), (5,3), (4,5)] representa o polinomio 2x^3+ 3x^4+5x^3
-}

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a)
--de forma a que (conta n p) indica quantos monomios de grau n existem em p.
conta :: Int -> Polinomio -> Int 
conta n [] = 0
conta n ((x,y):t) | n == y = 1 + conta n t
                  | otherwise = conta n t

--b)
--que indica o grau de um polin ́omio.
grau :: Polinomio -> Int 
grau [] = 0
grau ((x,y):t) | y > grau t = y
               | otherwise = grau t

--c)
--que selecciona os mon ́omios com um dado grau de um polin ́omio.
selgrau :: Int -> Polinomio -> Polinomio 
selgrau n [] = []
selgrau n ((x,y):t) | n == y = (x,y) : selgrau n t
                    | otherwise = selgrau n t

--d)
--que calcula a derivada de um polin ́omio.
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c,g):t)
    | g == 0 = deriv t
    | otherwise = (c * fromIntegral g,g-1) : deriv t --Como o c é um Double, fromIntegral converte g de Int para Double

--e) calcula o valor de um polin ́omio para uma dado valor de x.
calcula :: Float -> Polinomio -> Float
calcula n [] = 0
calcula n ((x,y):t) = x * (n ^ y) + calcula n t

--f) que retira de um polin ́omio os mon ́omios de coeficiente zero.
simp :: Polinomio -> Polinomio
simp [] = []
simp ((x,y):t) | x == 0 = simp t
               | otherwise = (x,y) : simp t

--g)que calcula o resultado da multiplica ̧c ̃ao de um mon ́omio por um polin ́omio.
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (x,y) ((x1,y1):t) = (x * x1, y + y1) : mult (x,y) t

--h)que dado um polin ́omio constr ́oi um polin ́omio equivalente em que n ̃ao podem aparecer varios mon ́omios com o mesmo grau.
normalizaAux :: Monomio -> Polinomio -> Polinomio
normalizaAux n [] = [n]
normalizaAux (x,y) ((h,t):t1) | y == t = (x+h,t) : t1 --Se for normalizaAux (x,y) t1 vai duplicar duas vezes, ficaria normaliza [(1,3),(2,3)] = [(3.0,3),(1.0,3)]
                              | otherwise = (h,t) : normalizaAux(x,y) t1
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((x,y):t) = normalizaAux (x,y) (normaliza t)

--i) faz a soma de dois polinomios de forma que se os polinomios que recebe estiverem normalizados produz tamb ́em um polin ́omio normalizado.
soma :: Polinomio -> Polinomio -> Polinomio
soma p [] = p
soma [] p = p
soma (h:t) p = soma t (somaAux h p)

somaAux :: Monomio -> Polinomio -> Polinomio
somaAux m [] = [m]
somaAux (cm,gm) ((c,g):t)
    | gm == g = (cm + c,g) : t
    | otherwise = (c,g) : somaAux (cm,gm) t

--j) calcula o produto de dois polinomios
produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (x:y) p = (mult x p) ++ produto y p

--k) que ordena um polon ́omio por ordem crescente dos graus dos seus monomios.
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (h:t) = ordenaAux h (ordena t) --Se colocar ordena t sem parentisis o haskell interpreta como (ordenaAux h ordena) t, a função pensa que estou a tentar aplicar a 3 argumentos

ordenaAux :: Monomio -> Polinomio -> Polinomio
ordenaAux _ [] = []
ordenaAux (x,y) ((x1,y1):t) | y1 > y = (x,y) : (x1,y1) : t
                            | otherwise = (x1,y1) : ordenaAux (x,y) t

--l) testa se dois polinomios sao equivalentes.
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)