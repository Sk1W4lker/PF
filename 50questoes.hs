--1) constrói a lista dos números inteiros compreendidos entre dois limites.
-- > enumFromTo 1 5 = [1,2,3,4,5]
enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 start end 
    | start > end = []
    | otherwise = start : enumFromTo1 (start + 1) end

--ou

asc :: Int -> Int -> [Int]
asc start end | start > end = []
              | start == end = [end]
              | start < end = start : asc (start + 1) end

--2) constrói a lista dos números inteiros compreendidos entre dois limites e espaçados de um valor constante.
-- > enumFromThenTo 1 3 10 = [1,3,5,7,9]
enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 start next end
    | start > end && next >= start || start < end && next < start = []
    | otherwise = start : enumFromThenTo1 next (next + (next-start)) end

--3) concatena duas listas
-- > (++) [1,2,3] [10,20,30] = [1,2,3,10,20,30]
plusPlus :: [a] -> [a] -> [a]
plusPlus [] l = l
plusPlus (h:t) l = h : plusPlus t l

--4) calcula o elemento da lista que se encontra nessa posição
-- > (!!) [10,20,30] 1 = 20
intInt :: [a] -> Int -> a
intInt (h:_) 0 = h
intInt (_:t) l = intInt t (l-1)

--5) dada uma lista calcula uma lista com os elementos dessa lista pela ordem inversa
-- > reverse [10,20,30] = [30,20,10]
reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (h:t) = t ++ reverse [h]
--ou reverse1 t ++ [h]

--6) > take 2 [10,20,30] = [10,20]
take1 :: Int -> [a] -> [a]
take1 0 (h:t) = t
take1 _ [] = []
take1 l (h:t) = h : take1 (l-1) t
--ou
--take _ [] = []
--take n (h:t)
--   | n <= 0 = []
--   | otherwise = h : take (n - 1) t

--7) > drop 2 [10,20,30] = [30]
drop1 :: Int -> [a] -> [a]
drop1 0 l = l
drop1 _ [] = []
drop1 l (h:t) = drop1 (l-1) t

--8)> zip [1,2,3] [10,20,30,40] = [(1,10),(2,20),(3,30)]
zip1 :: [a] -> [b] -> [(a,b)]
zip1 _ [] = []
zip1 [] _ = []
zip1 (h:t) (h1:t1) = (h,h1) : zip1 t t1 

--9) > replicate 3 10 = [10,10,10]
replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n x = x : replicate (n - 1) x

--10)> intersperce 1 [10,20,30] = [10,1,20,1,30]
intersperse1 :: a -> [a] -> [a]
intersperse1 l [] = []
intersperse1 l (h:t) | length (h:t) == 1 = [h]
                     | otherwise = h : l : intersperse1 l t
--ou
--intersperse :: a -> [a] -> [a]
--intersperse _ [] = []
--intersperse _ [h] = [h]
--intersperse x (h:t) = h : x : intersperse

--11) > group [1,2,2,3,4,4,4,5,4] = [[1],[2,2],[3],[4,4,4],[5],[4]]
group1 :: Eq a => [a] -> [[a]]
group1 [] = []
group1 [x] = [[x]]
group1 (h:t) | h == head t = (h : (head (group1 t))) : tail (group1 t) -- a unica diferença de head t para head group t é que head t é 2 e head group t é [2]
             | otherwise = [h] : group1 t 

--12) > concat [[1],[2,2],[3],[4,4,4],[5],[4]] = [1,2,2,3,4,4,4,5,4]
concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (h:t) = h ++ concat1 t

--13) > inits [11,21,13] = [[],[11],[11,21],[11,21,13]]
inits1 :: [a] -> [[a]]
inits1 [] = [[]]
inits1 l = (inits1 (init l)) ++ [l]

{-
inits [11,21,13]
inits [11,21] ++ [11,21,13]
inits [11] ++ [11,21] ++ [11,21,13]
inits [] ++ [11] ++ [11,21] ++ [11,21,13]
[[],[11],[11,21],[11,21,13]]
-}

--14) > tails [1,2,3] = [[1,2,3],[2,3],[3],[]]
tails :: [a] -> [[a]]
tails [] = [[]]
tails x = [x] ++ tails(tail x)

--15) > heads [[2,3,4],[1,7],[],[8,5,3]] = [2,1,8]
heads :: [[a]] -> [a]
heads [] = []
heads ([]:t) = heads t
heads (h:t) = (head h) : heads t

--16) > total [[2,3,4],[1,7],[],[8,5,3]] = 8
total :: [[a]] -> Int
total [] = 0
total (h:t) = length h + total t

--17) > fun [("rui",3,2), ("maria",5,2), ("ana",43,7)] = [("rui",2), ("maria",2), ("ana",7)]
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):t) = (a,c) : fun t

--18) > cola [("rui",3,2), ("maria",5,2), ("ana",43,7)] = "ruimariaana"
cola :: [(String,b,c)] -> String
cola [] = ""
cola ((a,b,c):t) = a ++ cola t

--19) > idade 2021 26 [("rui",1995), ("maria",2009), ("ana",1947)] = ["rui","ana"]
idade :: Int -> Int -> [(String,Int)] -> [String]
idade a b [] = []
idade ano idade2 ((nome,ano2):t) | (ano - ano2) >= 26 = nome : idade ano idade2 t
                                 | otherwise = idade ano idade2 t

--20) > powerEnumFrom 2 10 = [1,2,4,8,16,32,64,128,256,512] 
--que dado um valor n e um valor m constrói a lista [n^0,…,n^(m−1)]
powerEnumFrom2 :: Int -> Int -> [Int]
powerEnumFrom2 n 1 = [1]
powerEnumFrom2 n m | m <= 0 = []
                   | otherwise = powerEnumFrom2 n (m-1) ++ [n^(m-1)]

--21) se esse número é primo
isPrime :: Int -> Bool
isPrime n = isPrime2 n 2

isPrime2 :: Int -> Int -> Bool
isPrime2 n m | m^2 > n && m >= 2 = True
             | mod n m == 0 = False
             | otherwise = isPrime2 n (m+1)

--m ≤ √n
--m^2 <= √n^2
--m^2 <= n 
--Então seria verdade para m^2 > n 

--22) > isPrefixOf [10,20] [10,20,30] = True > isPrefixOf [10,30] [10,20,30] = False
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (h:t) (h2:t2) | h == h2 = isPrefixOf t t2
                         | otherwise = False

--23) > isSuffixOf [20,30] [10,20,30] = True > isSuffixOf [10,30] [10,20,30] = False
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf (h:t) (h2:t2) = h == head t2 && isSuffixOf t t2


--24) > isSubsequenceOf [20,40] [10,20,30,40] = True > isSubsequenceOf [40,20] = [10,20,30,40]
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (h:t) (h1:t2) = h == h1 && isSubsequenceOf t t2 || isSubsequenceOf (h:t) t2
                         
--25) calcula a lista de posições em que um dado elemento ocorre numa lista. > elemIndices 3 [1,2,3,4,3,2,3,4,5] = [2,4,6]
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices a l = elemConta a l 0

elemConta :: Eq a => a -> [a] -> Int -> [Int]
elemConta a [] n = []
elemConta a (h:t) n | a == h = n : elemConta a t (n+1)
                    | otherwise = elemConta a t (n+1)

--26) > nub [1,2,1,2,3,1,2] = [1,2,3]
nub :: Eq a => [a] -> [a]
nub [] = []
nub (h:t) | h `elem` t = nub t 
          | otherwise = h : nub t

--27) > delete 2 [1,2,1,2,3,1,2] = [1,1,2,3,1,2] > delete 4 [1,3,2,1,3,5] = [1,3,2,1,3,5]
delete :: Eq a => a -> [a] -> [a]
delete a [] = []
delete a (h:t) | a == h = t
               | otherwise = h : delete a t

--28) > (\\) [1,2,3,4,5,1] [1,5] = [2,3,4,1]
remove :: Eq a => [a] -> [a] -> [a]
remove l [] = l
remove [] _ = []
remove l (h:t) = remove (delete h l) t

--delete, deleta a primeira ocurrencia de um numero expecifico

--29) > union [1,1,2,3,4] [1,5] = [1,1,2,3,4,5]
union :: Eq a => [a] -> [a] -> [a]
union l [] = l
union l (h:t)
    | h `elem` l = union l t
    | otherwise = union (l ++ [h]) t

--Importate
--É assim que se adiciona a ultima linha o elemento

--30) > intersect [1,1,2,3,4] [1,3,5] = [1,1,3]
intersect :: Eq a => [a] -> [a] -> [a]
intersect l [] = l
intersect [] l = []
intersect (h:t) l | h `elem` l = h : intersect t l
                  | otherwise = intersect t l 

--31) > insert 25 [1,20,30,40] = [1,20,25,30,40]
inserte :: Ord a => a -> [a] -> [a]
inserte n [] = [n]
inserte n (h:t) | n < h = n : h : t
                | otherwise = h : inserte n t

inserte' :: Ord a => a -> [a] -> [a]
inserte' n [] = []
inserte' n (h:t) | n > h && n < (head t) = h : n : inserte' n t
                 | otherwise = h : inserte' n t

--32) > unwords ["Programacao", "Funcional"] = "Programacao Funcional"
unwords1 :: [String] -> String
unwords1 [] = ""
unwords1 (h:t) = h ++ (if null t then "" else " ") ++ unwords1 t

--33) > unlines ["Prog", "Func"] = "Prog\nFunc\n"
unlines1 :: [String] -> String
unlines1 [] = ""
unlines1 (h:t) = h ++ "\n" ++ unlines1 t

--34) retorna a posição onde se encontra o maior elemento > pMaior [2,6,3,1,5,4] = 1 > pMaior [30,10,20,40,0] = 3
pMaior :: Ord a => [a] -> Int
pMaior [_] = 0
pMaior (h:t)
    | h >= (t !! (pMaior t)) = 0
    | otherwise = 1 + pMaior t

--35) > lookup 'a' [('a',1),('b',4),('c',5)] = Just 1
lookup1 :: Eq a => a -> [(a,b)] -> Maybe b
lookup1 a [] = Nothing
lookup1 a ((x,y):t) | a == x = Just y
                    | otherwise = lookup1 a t

--36) > preCrescente [3,7,9,6,10,22] = [3,7,9]
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente (h:t) | h > head t = [h]
                   | otherwise = h : preCrescente (head t:t)

preCrescente1 :: Ord a => [a] -> [a]
preCrescente1 [] = []
preCrescente1 [x] = [x]
preCrescente1 (h:t) | h <= (head t) = h : preCrescente1 t
                    | otherwise = [h]

--37) > iSort [3,1,2,5,4] = [1,2,3,4,5]
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = inserte h (iSort t)

--38) dadas duas strings, retorna True se e só se a primeira for menor do que a segunda
-- > menor "sai" "saiu" = True > menor "programacao" "funcional" = False
menor :: String -> String -> Bool
menor _ "" = False
menor "" _ = True
menor a b | (length a) < (length b) = True
          | otherwise = False

--39) > elemMSet ’a’ [(’b’,2), (’a’,4), (’c’,1)] = True > elemMSet ’d’ [(’b’,2), (’a’,4), (’c’,1)] = False
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a ((x,y):t) | a == x = True
                     | otherwise = elemMSet a t 

--40) > converteMSet [(’b’,2), (’a’,4), (’c’,1)] = "bbaaaac"
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,1):t) = x : converteMSet t
converteMSet ((x,y):t) = x : converteMSet ((x,y-1) : t)

--41) > insereMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] = [(’b’,2),(’a’,4), (’c’,2)]
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((x,y):t) | a == x = (x,y+1) : t
                       | otherwise = (x,y) : insereMSet a t

--42) > removeMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] = [(’b’,2),(’a’,4)]
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet n [] = []
removeMSet n ((a,1):t) = removeMSet n t
removeMSet n ((a,e):t) | n == a = (a,(e-1)) : t
                       | otherwise = (a,e) : removeMSet n t

{-
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = []
removeMSet a ((x,y):t) | a == x && y > 1 = (x,y-1) : t
                       | a == x && y == 1 = t
                       | otherwise = (x,y) : removeMSet a t
-}

--43) > constroiMSet "aaabccc" = [(’a’,3), (’b’,1), (’c’,3)]
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (l:ls) = insereMSet l (constroiMSet ls)

--44) > partitionEithers [Left 1, Right 2, Left 3, Right 4, Left 5] = ([1,3,5],[2,4])
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers ((Left a):t) = (a : x, y)
    where (x,y) = partitionEithers t
partitionEithers ((Right a):t) = (x, a : y)
    where (x,y) = partitionEithers t

--45) > catMaybes [Just 1, Just 2, Nothing, Just 3, Nothing, Just 4, Just 5, Nothing] = [1,2,3,4,5]
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Nothing):t) = catMaybes t
catMaybes ((Just a):t) = a : catMaybes t

data Movimento = Norte | Sul | Este | Oeste
    deriving Show

--46) > caminho (0,0) (3,4) = [Este,Este,Este,Norte,Norte,Norte,Norte]
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (x1,y1) | x < x1 = Este : caminho (x+1,y) (x1,y1)     
                      | x > x1 = Oeste : caminho (x-1,y) (x1,y1)
                      | y < y1 = Norte : caminho (x,y+1) (x1,y1)
                      | y > y1 = Sul : caminho (x,y-1) (x1,y1)
                      | otherwise = []
--47) > hasLoops (0,0) [Norte, Norte, Este, Sul, Oeste, Sul, Este, Norte, Este] = True > hasLoops (2,1) [Sul, Este, Sul, Oeste, Norte, Este, Sul] = False
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x, y) (Norte:t) = posicao (x, y + 1) t
posicao (x, y) (Sul:t) = posicao (x, y - 1) t
posicao (x, y) (Este:t) = posicao (x + 1, y) t
posicao (x, y) (Oeste:t) = posicao (x - 1, y) t

hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops posi movs = posi == posicao posi movs || hasLoops posi (init movs)

--48) > contaQuadrados [Rect (0,0) (2,2), Rect (1,3) (7,4), Rect (5,2) (8,5), Rect (1,2) (2,4)] = 2
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados (h:t) | eQuadrado h = 1 + contaQuadrados t
                     | otherwise = contaQuadrados t

eQuadrado :: Rectangulo -> Bool
eQuadrado (Rect (x,y) (x1,y1)) = abs (y1 - y) == abs (x1 - x)

--49) > areaTotal [Rect (0,0) (2,2), Rect (1,3) (7,4), Rect (5,2) (8,5), Rect (1,2) (2,4)] = 21.0

areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1) (x2,y2)):t) = abs (x2 - x1) * abs (y2 - y1) + areaTotal t

--50) > naoReparar [Bom, Avariado, Bom, Razoavel, Avariado] = 3
data Equipamento = Bom | Razoavel | Avariado
    deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Bom:t) = 1 + naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t
naoReparar (Avariado:t) = naoReparar t

