import System.Random
--1)
--a)
inits :: [a] -> [[a]]
inits [] = [[]]
inits (h:t) = [] : map (h:) (inits t)

--b)
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (h:t) (h':t') = h == h' && isPrefixOf t t'

--2)
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

--(a)
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node x Empty Empty) = 1
folhas (Node x e d) = folhas e + folhas d

--b)
path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node x _ _) = [x]
path (False:t) (Node x e _) = x : path t e
path (True:t) (Node x _ d) = x : path t d

--3)
type Polinomio = [Coeficiente]
type Coeficiente = Float

--a)
valor :: Polinomio -> Float -> Float
valor [] n = 0
valor l n = valorAux l n 0

valorAux :: Polinomio -> Float -> Int -> Float
valorAux [] n acc = 0
valorAux (h:t) n acc = h*n^acc + valorAux t n (acc+1)

valor' :: Polinomio -> Float -> Float
valor' [] n = 0
valor' l n = foldr(\x acc -> x +n*acc) 0 l

deriv :: Polinomio -> Polinomio 
deriv p = map (\ (c,g) -> c*g) (zip [1..] (tail p))

--c) 
soma :: Polinomio -> Polinomio -> Polinomio
soma p q = zipWith (+) p1 p2 
    where n = max (length p) (length q)
          p1 = take n (p ++ repeat 0)
          p2 = take n (q ++ repeat 0)

--4 
--a) 
quebraLinha :: [Int] -> [a] -> [[a]]
quebraLinha [] _ = []
quebraLinha _ [] = []
quebraLinha (h:t) l = take h l : quebraLinha t (drop h l)

--b) 
fragmenta :: [Int] -> [Int] -> Mat a -> [Mat a]
fragmenta [] _ _ = []
fragmenta (h:t) l m = fragmenta' l (take h m) ++ fragmenta t l (drop h m)

-- Função que separa cada lista da matriz num certo indice
fragmenta' :: [Int] -> Mat a -> [Mat a]
fragmenta' [] _ = []
fragmenta' (h:t) m = map (take h) m : fragmenta' t (map (drop h) m)

--c) 
type Mat a = [[a]]

geraMat :: (Int,Int) -> (Int,Int) -> IO (Mat Int)
geraMat (x,y) (a,b) = do 
    let l = sequence (replicate y (randomRIO (a,b)))
    sequence (replicate x l)

{-
main :: IO ()
main = do
    putStrLn "Dimensoes (linhas,colunas):"
    dimsStr <- getLine
    let dims = read dimsStr :: (Int,Int)

    putStrLn "Intervalo (min,max):"
    intervaloStr <- getLine
    let intervalo = read intervaloStr :: (Int,Int)

    m <- geraMat dims intervalo
    print m
-}