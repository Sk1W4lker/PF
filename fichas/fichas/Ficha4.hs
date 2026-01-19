module Ficha4 where
import Data.Char (intToDigit)
import Data.Char 
{-
# 1. Para cada uma das express ̃oes seguintes, exprima por enumera ̧c ̃ao a lista correspon-
dente. Tente ainda, para cada caso, descobrir uma outra forma de obter o mesmo
resultado.
(a) [x | x <- [1..20], mod x 2 == 0, mod x 3 == 0] 
R: [6,12,18]

(b) [x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0] 
R: [6,12,18]

(c) [(x,y) | x <- [0..20], y <- [0..20], x+y == 30] 
R: [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]

(d) [sum [y | y <- [1..x], odd y] | x <- [1..10]] 
R: [1,1,4,4,9,9,16,16,25,25]

x = 1, ímpares = [1], soma = 1
x = 2, ímpares = [1], soma = 1
x = 3, ímpares = [1,3], soma = 4
x = 4, ímpares = [1,3], soma = 4
x = 5, ímpares = [1,3,5], soma = 9
x = 6, ímpares = [1,3,5], soma = 9
x = 7, ímpares = [1,3,5,7], soma = 16
x = 8, ímpares = [1,3,5,7], soma = 16
x = 9, ímpares = [1,3,5,7,9], soma = 25
x = 10, ímpares = [1,3,5,7,9], soma = 25


# 2. Defina cada uma das listas seguintes por compreensao.

(a) [1,2,4,8,16,32,64,128,256,512,1024]
R: [2^x | x <- [0..10]]

(b) [(1,5),(2,4),(3,3),(4,2),(5,1)] 
R: [(x,y) | x <- [1..5], y <- [1..5], x+y == 6]

(c) [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]] 
R: [[1..x] | x <- [1..5]]

(d) [[1],[1,1],[1,1,1],[1,1,1,1],[1,1,1,1,1]] 
R: [replicate x 1 | x <- [1..5]]

(e) [1,2,6,24,120,720] 
R: [ product [y | y <- [1..x]] | x <- [1..6]]
-} 

{-
3)
Defina a fun ̧c ̃ao digitAlpha :: String -> (String,String), que dada uma string,
devolve um par de strings: uma apenas com as letras presentes nessa string, e a outra
apenas com os n ́umeros presentes na string. Implemente a fun ̧c ̃ao de modo a fazer uma
unica travessia da string. Relembre que as funcoes isDigit,isAlpha :: Char -> Bool
est ̃ao j ́a definidas no m ́odulo Data.Char.-}

digitAlpha :: String -> (String,String)
digitAlpha "" = ("","")
digitAlpha (h:t) | isAlpha h = (h:a,b)
                 | isDigit h = (a,h:b)
                 | otherwise = (a,b)
                 where (a,b) = digitAlpha t

{-
4)
Defina a fun ̧c ̃ao nzp :: [Int] -> (Int,Int,Int) que, dada uma lista de inteiros,
conta o n ́umero de valores nagativos, o n ́umero de zeros e o n ́umero de valores positivos,
devolvendo um triplo com essa informa ̧c ̃ao. Certifique-se que a fun ̧c ̃ao que definiu
percorre a lista apenas uma vez.-}
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h < 0 = (1+a,b,c)
          | h == 0 = (a,1+b,c)
          | h < 0 = (a,b,1+c)
          where (a,b,c) = nzp t

{-
5)
Defina a fun ̧c ̃ao divMod :: Integral a => a -> a -> (a, a) que calcula simultane-
amente a divis ̃ao e o resto da divis ̃ao inteira por subtrac ̧c ̃oes sucessivas.
-}
divMod1 :: Integral a => a -> a -> (a, a)
divMod1 x y | x - y < 0 = (0,x)
            | otherwise = (q+1,r)
            where (q,r) = divMod1 (x-y) y

{-
6) 
Otimize a seguinte definição recursiva da função que 
calcula o n-ésimo número da sequência de Fibonacci, usando
uma função auxiliar com 2 acumuladores que representam, 
respectivamente, o n-ésimo e o n+1-ésimo números dessa sequência.
-}
fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits l = fromDigitsAux l 0

fromDigitsAux :: [Int] -> Int -> Int
fromDigitsAux [] acc = acc
fromDigitsAux (h:t) acc = fromDigitsAux t (h + 10 * acc)

{-
7)
Utilizando uma funcao auxiliar com acumuladores, optimize seguinte defini ̧c ̃ao que
determina a soma do segmento inicial de uma lista com soma m ́axima.
-}
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maxSumInitAux l 0

maxSumInitAux :: (Num a, Ord a) => [a] -> a -> a
maxSumInitAux [] acc = acc  
maxSumInitAux l acc | sum(init l) > acc = maxSumInitAux (init l) (sum (init l))
                    | otherwise = maxSumInitAux (init l) acc

{- 
8)
Optimize a seguinte defini ̧c ̃ao recursiva da fun ̧c ̃ao que calcula o n- ́esimo n ́umero da
sequˆencia de Fibonacci, usando uma fun ̧c ̃ao auxiliar com 2 acumuladores que represen-
tam, respectivamente, o n- ́esimo e o n+1- ́esimo n ́umeros dessa sequˆencia.
-}
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fibAux (n-2) 0 1

fibAux :: Int -> Int -> Int -> Int
fibAux 0 n _ = n
fibAux i n n1 = fibAux (i - 1) n1 (n + n1)

{-
9) Defina a função intToStr :: Integer -> String que converte um inteiro numa string. Utilize uma função auxiliar
com um acumulador onde vai construindo a string que vai devolver no final
-}
intToStr :: Integer -> String
intToStr 0 = "0"
intToStr l = intToStrAux l ""

intToStrAux :: Integer -> String -> String
intToStrAux 0 acc = acc
intToStrAux n acc = intToStrAux (div n 10) (intToDigit (fromInteger (mod n 10)) : acc)

intToStr :: Integer -> String
intToStr 0 = "0"
intToStr l = foldl(\acc x -> intToStr (div x 10) (fromInteger (mod nx 10))) "" l

