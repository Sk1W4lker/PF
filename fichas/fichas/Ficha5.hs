module Ficha5 where 
import Data.Char
import Data.List 
{-
# 1

a)
any :: (a -> Bool) -> [a] -> Bool que teste se um predicado  ́e verdade para
algum elemento de uma lista; por exemplo:
any odd [1..10] == True
-}
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (h:t) = f h || any' f t

{-
b)
zipWith :: (a->b->c) -> [a] -> [b] -> [c] que combina os elementos de
duas listas usando uma fun ̧c ̃ao espec ́ıfica; por exemplo:
zipWith (+) [1,2,3,4,5] [10,20,30,40] == [11,22,33,44]
-}
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (h:t) (h1:t1) = f h h1 : zipWith' f t t1

{-
(c) takeWhile :: (a->Bool) -> [a] -> [a] que determina os primeiros elementos
da lista que satisfazem um dado predicado; por exemplo:
takeWhile odd [1,3,4,5,6,6] == [1,3].
-}
takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (h:t) | f h = h : takeWhile' f t
                   | otherwise = []

{-
(d) dropWhile :: (a->Bool) -> [a] -> [a] que elimina os primeiros elementos da
lista que satisfazem um dado predicado; por exemplo:
dropWhile odd [1,3,4,5,6,6] == [4,5,6,6].
-}
dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (h:t) | f h = dropWhile' f t
                   | otherwise = t

{-
(e) span :: (a-> Bool) -> [a] -> ([a],[a]), que calcula simultaneamente os dois
resultados anteriores. Note que apesar de poder ser definida `a custa das outras
duas, usando a defini ̧c ̃ao
span p l = (takeWhile p l, dropWhile p l)
nessa defini ̧c ̃ao h ́a trabalho redundante que pode ser evitado. Apresente uma
defini ̧c ̃ao alternativa onde n ̃ao haja duplica ̧c ̃ao de trabalho.
-}
span' :: (a-> Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f (h:t) | f h = (h:a,b)
              | otherwise = ([],(h:t))
              where (a,b) = span' f t

{-
(f) deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a] que apaga o primeiro el-
emento de uma lista que  ́e “igual” a um dado elemento de acordo com a fun ̧c ̃ao
de compara ̧c ̃ao que  ́e passada como parˆametro. Por exemplo:
deleteBy (\x y -> snd x == snd y) (1,2) [(3,3),(2,2),(4,2)] == [(3,3),(4,2)]
-}
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f _ [] = []
deleteBy' f x (h:t) | f x h = t
                    | otherwise = h : deleteBy' f x t

{-
(g) sortOn :: Ord b => (a -> b) -> [a] -> [a] que ordena uma lista compara-
ndo os resultados de aplicar uma fun ̧c ̃ao de extrac ̧c ̃ao de uma chave a cada ele-
mento de uma lista. Por exemplo:
sortOn fst [(3,1),(1,2),(2,5)] == [(1,2),(2,5),(3,1)].
-}
sortOn' :: Ord b => (a -> b) -> [a] -> [a] 
sortOn' _ [] = []
sortOn' f (h:t) = insertOn' f h (sortOn' f t) 

-- Função que insere um elemento numa lista ordenada 
insertOn' :: Ord b => (a -> b) -> a -> [a] -> [a] 
insertOn' _ x [] = [x] 
insertOn' f x (h:t) | f x > f h = h : insertOn' f x t  
                    | otherwise = x : h : t 

--2)
type Polinomio = [Monomio]
type Monomio = (Float,Int)

{-
(a) selgrau :: Int -> Polinomio -> Polinomio que selecciona os mon ́omios com
um dado grau de um polin ́omio.
-}
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau n l = filter(\(x,y) -> n == y) l

selgrau' :: Int -> Polinomio -> Polinomio
selgrau' _ [] = []
selgrau' n l = filter(\x -> n == snd x) l

selgrau1 :: Int -> Polinomio -> Polinomio
selgrau1 _ [] = []
selgrau1 n l = filter aux l
        where aux(x,y) = y == n

{-
(b) conta :: Int -> Polinomio -> Int de forma a que (conta n p) indica quan-
tos mon ́omios de grau n existem em p.
-}
conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n l = length $ filter(\x -> n == snd x) l

conta' :: Int -> Polinomio -> Int
conta' n [] = 0
conta' n l = foldr(\(x,y) r -> if n == y then 1 + r else r) 0 l

{-
(c) grau :: Polinomio -> Int que indica o grau de um polin ́omio.
-}
grau :: Polinomio -> Int
grau n = maximum $ map snd n

grau' :: Polinomio -> Int
grau' n = foldl(\acc x -> if acc > snd x then acc else snd x) 0 n

{-
(d) deriv :: Polinomio -> Polinomio que calcula a derivada de um polin ́omio.
-}
deriv :: Polinomio -> Polinomio
deriv n = map(\(x,y) -> (x * fromIntegral y, y-1)) $ filter(\(x,y) -> y /= 0) n

deriv' :: Polinomio -> Polinomio
deriv' n = foldl(\acc (x,y) -> (x * fromIntegral y, y-1) : acc) [] n

{-
(e) calcula :: Float -> Polinomio -> Float que calcula o valor de um polin ́omio
para uma dado valor de x
-}
calcula :: Float -> Polinomio -> Float
calcula n p = foldl(\acc (x,y) -> (x*n^y) + acc) 0 p

{-
(f) simp :: Polinomio -> Polinomio que retira de um polin ́omio os mon ́omios de
coeficiente zero.
-}
simp :: Polinomio -> Polinomio
simp n = filter(\(x,y) -> x /= 0) n

{-
(g) mult :: Monomio -> Polinomio -> Polinomio que calcula o resultado da mul-
tiplica ̧c ̃ao de um mon ́omio por um polin ́omio.
-}
mult :: Monomio -> Polinomio -> Polinomio
mult (x,y) p = map (\(x1,y1) -> (x*x1,y+y1)) p

mult' :: Monomio -> Polinomio -> Polinomio
mult' n p = map (\(x,y) -> ((fst n)*x,(snd n)+y)) p

{-
(h) ordena :: Polinomio -> Polinomio que ordena um polon ́omio por ordem cres-
cente dos graus dos seus mon ́omios.
-}
ordena :: Polinomio -> Polinomio
ordena = sortOn' snd

{-
(i) normaliza :: Polinomio -> Polinomio que dado um polin ́omio constr ́oi um
polin ́omio equivalente em que n ̃ao podem aparecer varios mon ́omios com o mesmo
grau
-}
normaliza :: Polinomio -> Polinomio
normaliza n = foldl (\acc (x,y) -> adiciona (x,y) acc) [] n
            where adiciona :: Monomio -> Polinomio -> Polinomio
                  adiciona x [] = [x]
                  adiciona (x,y) ((x1,y1):t) | y == y1 = (x+x1,y1) : t
                                             | otherwise = (x,y) : adiciona (x,y) t

{-
(j) soma :: Polinomio -> Polinomio -> Polinomio que faz a soma de dois polin ́omios
de forma que se os polin ́omios que recebe estiverem normalizados produz tamb ́em
um polin ́omio normalizado.
-}
soma :: Polinomio -> Polinomio -> Polinomio
soma n p = foldl (\acc (x,y) -> adiciona (x,y) acc) n p
            where adiciona :: Monomio -> Polinomio -> Polinomio
                  adiciona x [] = [x]
                  adiciona (x,y) ((x1,y1):t) | y == y1 = (x+x1,y1) : t
                                             | otherwise = (x,y) : adiciona (x,y) t

{-
(k) produto :: Polinomio -> Polinomio -> Polinomio que calcula o produto de
dois polin ́omios
-}
produto :: Polinomio -> Polinomio -> Polinomio
produto n p = foldl(\acc m -> soma(mult m p) acc) [] n

produto' :: Polinomio -> Polinomio -> Polinomio 
produto' p1 p2 = normaliza $ concat $ map ( `mult` p2) p1

{-
(l) equiv :: Polinomio -> Polinomio -> Bool que testa se dois polin ́omios s ̃ao
equivalentes.
-}
equiv :: Polinomio -> Polinomio -> Bool 
equiv p p' = ordena (normaliza p) == ordena (normaliza p')

--3. Considere a sequinte defini ̧c ̃ao para representar matrizes:
type Mat a = [[a]]
--Ex: [[1,2,3], [0,4,5], [0,0,6]]

--a)
dimOK :: Mat a -> Bool
dimOK = (== 1) . length . nub . map length
--length da o tamanho [3,3,3]
--nub remove repetidos [3]
--(==1) ve se sem repetidos só tem um elemento [3] == 1

dimOK' :: Mat a -> Bool
dimOK' [] = False
dimOK' m = primeiro /= 0 && resto == []
   where tamanho = map length m
         primeiro = head tamanho
         resto = filter(/= primeiro) (tail tamanho)

--(b) dimMat :: Mat a -> (Int,Int) que calcula a dimens ̃ao de uma matriz
dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat n = (length n, maximum(map length n)) --ou dimMat mat = (length mat, length (head mat))

--(c) addMat :: Num a => Mat a -> Mat a -> Mat a que adiciona duas matrizes.
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat n p = zipWith(zipWith(+)) n p

--(d) transpose :: Mat a -> Mat a que calcula a transposta de uma matriz.
transpose' :: Mat a -> Mat a
transpose' [] = []
transpose' ([]:t) = []
transpose' n = map head n : transpose' (map tail n)

--(e) multMat :: Num a => Mat a -> Mat a -> Mat a que calcula o produto de duas matrizes.
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat (l:ls) m = (linha l m) : multMat ls m

linha :: Num a => [a]-> Mat a -> [a]
linha l ([]:_) = [ ]
linha l m = sum (zipWith' (*) l (map head m)) : linha l (map tail m)

--(f) zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c que, `a semelhan ̧ca do que acontece com a fun ̧c ̃ao zipWith, combina duas matrizes. Use essa fun ̧c ̃ao para definir uma fun ̧c ̃ao que adiciona duas matrizes
zipWhat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c 
zipWhat f m m' = zipWith (zipWith f) m m'

--(g) triSup :: Num a => Mat a -> Bool que testa se uma matriz quadrada  ́e triangular superior (i.e., todos os elementos abaixo da diagonal s ̃ao nulos).
triSup :: (Eq a,Num a) => Mat a -> Bool 
triSup m = nub (concat (map (\ l -> take (index m l) l) m)) == [0]

--Determina o indice de um elemento de uma lista.
index :: Eq a => [a] -> a -> Int 
index (h:t) n | n == h = 0 
              | otherwise = 1 + index t n 

--(h) rotateLeft :: Mat a -> Mat a que roda uma matriz 90o para a esquerda. Por exemplo, o resultado de rodar a matriz acima apresentada deve corresponder `a matriz
rotateLeft :: Mat a -> Mat a 
rotateLeft m = transpose $ map reverse m 