import Data.Char

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices _ [] = []
elemIndices n l = elemAux n 0 l

elemAux :: Eq a => a -> Int -> [a] -> [Int]
elemAux n acc [] = []
elemAux n acc (h:t) | n == h = acc : elemAux n (acc+1) t
                    | otherwise = elemAux n (acc+1) t

elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' e (x:xs) | e == x    = 0 : map (+1) (elemIndices' e xs)
                      | otherwise =     map (+1) (elemIndices' e xs)

--b)
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (h:t) (h':t') = h == h' && isSubsequenceOf t t' || isSubsequenceOf (h:t) t'

--2)
data BTree a = Empty | Node a (BTree a) (BTree a)
--a)
lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP _ Empty = Nothing
lookupAP x (Node (k,v) esq dir)
    | x == k    = Just v
    | x < k     = lookupAP x esq
    | otherwise = lookupAP x dir

--b) 
zipWithBT :: (a->b->c) -> BTree a -> BTree b-> BTree c
zipWithBT _ Empty _ = Empty
zipWithBT _ _ Empty = Empty
zipWithBT f (Node x e d) (Node x' e' d') = Node (f x x') (zipWithBT f e e') (zipWithBT f d d')

--3)
digitAlpha :: String -> (String,String)
digitAlpha "" = ("","")
digitAlpha (h:t) | isAlpha h = (h:a,b)
                 | isDigit h = (a,h:b)
                 | otherwise = (a,b)
                 where (a,b) = digitAlpha t

--4)
data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)
--a)
firstSeq :: Seq a -> a
firstSeq (Cons x _)   = x
firstSeq (App s1 _)   = firstSeq s1

--b)
dropSeq :: Int -> Seq a -> Seq a
dropSeq 0 (Cons x l) = l
dropSeq n (Cons x l) = dropSeq (n-1) l
dropSeq n (App l l') = App (dropSeq n l) (dropSeq (n-countSeq l) l')

countSeq :: Seq a -> Int
countSeq (Nil) = 0
countSeq (Cons x l) = 1 + countSeq l
countSeq (App l l') = countSeq l + countSeq l'

--c)
instance Show a => Show (Seq a) where
    show = showSeq

showSeq :: Show a => Seq a -> String
showSeq l = "<<" ++ showElems (showLista l) ++ ">>"

showElems :: Show a => [a] -> String
showElems [] = ""
showElems [x] = show x
showElems (h:t) = show h ++ "," ++ showElems t

showLista :: Seq a -> [a]
showLista Nil = []
showLista (Cons x l) = x : showLista l
showLista (App l l') = showLista l ++ showLista l'


--5. Considere a sequinte defini ̧c ̃ao para representar matrizes: type Mat a = [[a]]
--(a) Defina a fun ̧c ̃ao getElem :: Mat a -> IO a, que selecciona aleatoriamente um elemento da matriz.
getElem :: Mat a -> IO a
getElem l = do 
        let nLinhas = length l
        i <- randomRIO(0,nLinhas - 1)
        
        let linha = l !! i
        let nColunas = length linha
        j <- randomRIO(0, nColunas - 1)

        return (linha !! j) 

--Assumindo todos os lados serem iguais
getElemPerf :: Mat a -> IO a
getElemPerf l = do
        i <- randomRIO(0, length l - 1)
        j <- randomRIO(0, length(head l) - 1)
        return (l !! i !! j)

--b)
type Mat a = [[a]]

magic :: Mat Int -> Bool
magic [] = True
magic m  = all (==h) t
    where (h:t) = di (tran m) ++ di m ++ linhas m ++ linhas (tran m)

di :: Mat Int -> [Int]
di m = [sum [ m !! k !! k | k <- [0..length m -1]]]

tran :: Mat Int -> Mat Int
tran m = [ map (!!k) m | k <- [0..length m -1]]

linhas :: Mat Int -> [Int]
linhas m = map sum m 