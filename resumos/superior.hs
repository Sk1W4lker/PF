-- 1. Apresente definições das seguintes funções de ordem superior, já pré-definidas no Prelude ou no Data.List:

{-
(a) any :: (a -> Bool) -> [a] -> Bool que teste se um predicado ´e verdade para
algum elemento de uma lista; por exemplo:
any odd [1..10] == True
-}
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (h:t) = f h || any' f t

{-
(b) zipWith :: (a->b->c) -> [a] -> [b] -> [c] que combina os elementos de
duas listas usando uma fun¸c˜ao espec´ıfica; por exemplo:
zipWith (+) [1,2,3,4,5] [10,20,30,40] == [11,22,33,44].
-}
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f _ [] = []
zipWith' f [] _ = []
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
                   | otherwise = h : t

{-
(e) span :: (a-> Bool) -> [a] -> ([a],[a]), que calcula simultaneamente os dois
resultados anteriores. Note que apesar de poder ser definida `a custa das outras
duas, usando a defini¸c˜ao
span p l = (takeWhile p l, dropWhile p l)
nessa defini¸c˜ao h´a trabalho redundante que pode ser evitado. Apresente uma
defini¸c˜ao alternativa onde n˜ao haja duplica¸c˜ao de trabalho.
-}
span' :: (a-> Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f (h:t) | f h = (h:x,y)
              | otherwise = ([],(h:t))
              where (x,y) = span' f t

{-
(f) deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a] que apaga o primeiro el-
emento de uma lista que ´e “igual” a um dado elemento de acordo com a fun¸c˜ao
de compara¸c˜ao que ´e passada como parˆametro. Por exemplo:
deleteBy (\x y -> snd x == snd y) (1,2) [(3,3),(2,2),(4,2)]
-}
deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy f _ [] = []
deleteBy f n (h:t) | f n h = t
                   | otherwise = h : deleteBy f n t

{-
(g) sortOn :: Ord b => (a -> b) -> [a] -> [a] que ordena uma lista compara-
ndo os resultados de aplicar uma fun¸c˜ao de extrac¸c˜ao de uma chave a cada ele-
mento de uma lista. Por exemplo:
sortOn fst [(3,1),(1,2),(2,5)] == [(1,2),(2,5),(3,1)].
-}

insertOn :: Ord b => (a -> b) -> a -> [a] -> [a]
insertOn f n [] = [n]
insertOn f n (h:t) | f n > f h = h : insertOn f n t
                   | otherwise = n : h : t

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f (h:t) = insertOn f h (sortOn f t)

{--
insertOn assume que a lista onde insere já está ordenada.
Ele compara o elemento a inserir com os elementos da lista
e PÁRA na primeira posição onde a ordem falha.

Se usarmos apenas t (lista não ordenada),
insertOn pode parar cedo demais e não ver elementos mais pequenos
que aparecem mais à frente, produzindo uma lista não ordenada.

Por isso, em sortOn é obrigatório usar (sortOn f t),
garantindo que a inserção é feita numa lista já ordenada.

--Exemplo função que ordena sem superior.

insere :: Ord a => a -> [a] -> [a]
insere n [] = [n]
insere n (h:t) | n > h = h : insere n t
               | otherwise = n : h : t

ordena :: Ord a => [a] -> [a]
ordena [] = []
ordena (h:t) = insere h (ordena t)
--}

{-
2. Relembre a quest˜ao sobre polin´omios introduzida na Ficha 3, onde um polin´omio era
representado por uma lista de mon´omios representados por pares (coeficiente, expoente)
-}
type Polinomio = [Monomio]
type Monomio = (Float,Int)

{-
Por exemplo, [(2,3), (3,4), (5,3), (4,5)] representa o polin´omio 2x^3 + 3x^4 +
5x^3 + 4x^5. Redefina as fun¸c˜oes pedidas nessa ficha, usando agora fun¸c˜oes de ordem
superior (definidas no Prelude ou no Data.List) em vez de recursividade expl´ıcita:
-}

--(a) selgrau :: Int -> Polinomio -> Polinomio que selecciona os mon´omios com um dado grau de um polin´omio
selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n l = filter(\(x,y) -> n == y) l

--(b) conta :: Int -> Polinomio -> Int de forma a que (conta n p) indica quantos monomios de grau n existem em p.
conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n l = length $ filter(\(x,y) -> n == y) l --length(filter(\(x,y) -> n == y) l)
--length . (filter(\(x,y) -> n == y) l)

{-
conta' :: Int -> Polinomio -> Int
conta' n [] = 0
conta' n l = foldr(\(x,y) r -> if n == y then 1 + r else r) 0 l -- Foldr tem acumulador.
-}

--(c) grau :: Polinomio -> Int que indica o grau de um polin´omio.
grau :: Polinomio -> Int
grau [] = 0
grau l = maximum $ map snd l --maximum $ map(\(x,y) -> y) l

grau' :: Polinomio -> Int
grau' [] = 0
grau' l = foldr(\(x,y) r -> if y > r then y else r) 0 l --com acumulador a direita

grau1 :: Polinomio -> Int
grau1 [] = 0
grau1 l = foldl(\r (x,y) -> if y > r then y else r) 0 l --com acumulador a esquerda

--(d) deriv :: Polinomio -> Polinomio que calcula a derivada de um polin´omio.
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv l = map(\(x,y) -> (x * fromIntegral y,(y-1)) ) $ filter(\(x,y) -> y /= 0) l

deriv' :: Polinomio -> Polinomio 
deriv' [] = []
deriv' l = foldr(\(x,y) acc -> (x * fromIntegral y,(y-1)) : acc) [] l

{-
deriv1 :: Polinomio -> Polinomio 
deriv1 [] = []
deriv1 l = foldl(\acc (x,y) -> (x * fromIntegral y,(y-1)) : acc) [] l

[(2,3), (3,4), (5,3), (4,5)] == [(20.0,4),(15.0,2),(12.0,3),(6.0,2)] vira ao contrário.
-}
--(e) calcula :: Float -> Polinomio -> Float que calcula o valor de um polin´omio para uma dado valor de x.
calcula :: Float -> Polinomio -> Float
calcula n [] = 0
calcula n l = foldr(\(x,y) r -> x*n^y + r) 0 l -- sum (map(\(x,y) -> x*n^y) l)

{-
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

calcula 3 [(2,2),(1,1),(4,0)]
f (2,2) (f (1,1) (f (4,0) (foldr f 0 [])))
-}

--(f) simp :: Polinomio -> Polinomio que retira de um polin´omio os mon´omios de coeficiente zero.
simp :: Polinomio -> Polinomio
simp l = filter(\(x,y) -> y /= 0) l

--(g) mult :: Monomio -> Polinomio -> Polinomio que calcula o resultado da multiplicação de um monómio por um polinómio.
mult :: Monomio -> Polinomio -> Polinomio
mult n [] = []
mult n l = map(\(x,y) -> (x*fst n,y+snd n)) l --mult (x,y) p = map (\(x1,y1) -> (x*x1,y+y1)) p

--(h) ordena :: Polinomio -> Polinomio que ordena um polon´omio por ordem crescente dos graus dos seus mon´omios.
ordena :: Polinomio -> Polinomio
ordena p = sortOn snd p

{-
(i) normaliza :: Polinomio -> Polinomio que dado um polin´omio constr´oi um
polin´omio equivalente em que n˜ao podem aparecer varios mon´omios com o mesmo
grau.
-}
normaliza :: Polinomio -> Polinomio
normaliza l = foldl(\acc m -> adiciona m acc) [] l
        where adiciona :: Monomio -> Polinomio -> Polinomio
              adiciona n [] = [n]
              adiciona (x,y) ((c,g):t) | g == y = (x+c,g) : t 
                                       | otherwise = (c,g) : adiciona (x,y) t
{-
Como funciona o acumulador:
adiciona (2,1) [] = [(2,1)]
adiciona (3,2) [(2,1)] = [(2,1),(3,2)]
adiciona (4,1) [(2,1),(3,2)]
→ (2+4,1):(3,2) = [(6,1),(3,2)]
-}

{-
(j) soma :: Polinomio -> Polinomio -> Polinomio que faz a soma de dois polin´omios
de forma que se os polin´omios que recebe estiverem normalizados produz tamb´em
um polin´omio normalizado
-}
soma :: Polinomio -> Polinomio -> Polinomio 
soma p1 p2 = normaliza (p1 ++ p2)

soma' :: Polinomio -> Polinomio -> Polinomio
soma' p1 p2 = foldl (\acc m -> adiciona m acc) p1 p2
    where adiciona :: Monomio -> Polinomio -> Polinomio
          adiciona m [] = [m]
          adiciona (cm,gm) ((c,g):t) = if gm == g then (cm+c,g) : t else (c,g) : adiciona (cm,gm) t

{-
(k) produto :: Polinomio -> Polinomio -> Polinomio que calcula o produto de
dois polin´omios
-}
produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = foldl(\acc m -> soma(mult m p2) acc) [] p1

{-
Exemplo:

p1 = 2x³ + 4x⁵
p2 = x + 3

2x³ * x  = 2x⁴
2x³ * 3  = 6x³

4x⁵ * x  = 4x⁶
4x⁵ * 3  = 12x⁵

R: 2x⁴ + 6x³ + 4x⁶ + 12x⁵
-}

{-
(l) equiv :: Polinomio -> Polinomio -> Bool que testa se dois polin´omios s˜ao
equivalentes
-}
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena(normaliza p1) == ordena(normaliza p2)

{-
3. Considere a sequinte defini¸c˜ao para representar matrizes:
type Mat a = [[a]]

[1,2,3], [0,4,5], [0,0,6]]
Defina as seguintes fun¸c˜oes sobre matrizes (use, sempre que achar apropriado, fun¸c˜oes
de ordem superior).
-}

--(a) dimOK :: Mat a -> Bool que testa se uma matriz est´a bem constru´ıda (i.e., se todas as linhas tˆem a mesma dimens˜ao)
dimOK :: Mat a -> Bool
dimOK [] = False
dimOK l = primeiro /= 0 && resto == []
      where tamanho = map length l
            primeiro = head tamanho
            resto = filter(/=primeiro) (tail tamanho)

--(b) dimMat :: Mat a -> (Int,Int) que calcula a dimensão de uma matriz.
dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat n = (length n, maximum(map length n))

--(c) addMat :: Num a => Mat a -> Mat a -> Mat a que adiciona duas matrizes.
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat l l' = zipWith(zipWith(+)) l l'

--(d) transpose :: Mat a -> Mat a que calcula a transposta de uma matriz.
transpose :: Mat a -> Mat a
transpose [] = []
transpose ([]:t) = []
transpose n = (map head n) : transpose (map tail n)

--(e) multMat :: Num a => Mat a -> Mat a -> Mat a que calcula o produto de duas matrizes
multMat :: Num a => Mat a -> Mat a -> Mat a 
multMat m m' = map (\ l -> map sum (map (zipWith (*) l) z)) m 
    where z = transpose m' 

{-
(f) zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c que, `a semelhan¸ca
do que acontece com a fun¸c˜ao zipWith, combina duas matrizes. Use essa fun¸c˜ao
para definir uma fun¸c˜ao que adiciona duas matrizes.
-}
zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f l l' = zipWMat (zipWMat f) l l'

{-
(g) triSup :: Num a => Mat a -> Bool que testa se uma matriz quadrada ´e trian-
gular superior (i.e., todos os elementos abaixo da diagonal s˜ao nulos).
-}
triSup :: (Eq a,Num a) => Mat a -> Bool 
triSup m = nub (concat (map (\ l -> take (index m l) l) m)) == [0]

-- Função que determina o indice de um elemento de uma lista
index :: Eq a => [a] -> a -> Int 
index (h:t) n | n == h = 0 
              | otherwise = 1 + index t n 

-- h)                               
rotateLeft :: Mat a -> Mat a 
rotateLeft m = transpose $ map reverse m 

--Teste 25/26 Desenvolvimento
type Pol = [Float]

--a)
somaPol :: Pol -> Pol -> Pol
somaPol xs [] = xs
somaPol [] ys = ys
somaPol (h:t) (h1:t1) = (h+h1) : somaPol t t1

--b)
multPol :: Pol -> Pol -> Pol
multPol [] _ = []
multPol (h:t) ys = somaPol (map (h*) ys) (0 : multPol t ys) 

--Multipla
prefixos :: [a] -> [[a]]
prefixos [] = [[]]
prefixos (h:t) = [] : map (h:) (prefixos t)

{-
prefixos [x]
= [] : map (x:) (prefixos [])
= [] : map (x:) [[]]
= [] : [[x]]
= [[],[x]]
-}

sufixos :: [a] -> [[a]]
sufixos [] = [[]]
sufixos l@(h:t) = l : sufixos t
--sufixos [3] = [3] : sufixos [] = [3] : [[]] = [[3],[]]
