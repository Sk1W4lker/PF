{-
1. Considere o tipo MSet a para representar multi-conjuntos de tipo a
type MSet a = [(a,Int)]
Considere ainda que nestas listas n˜ao h´a pares cuja primeira componente coincida, nem cuja segunda
componente seja menor ou igual a zero. Para al´em disso, os multi-conjuntos est˜ao organizados
por ordem decrescente da muiltplicidade. O multi-conjunto {’b’,’a’,’c’,’b’,’b’,’a’,’b’} ´e
representado pela lista [(’b’,4),(’a’,2),(’c’,1)], por exemplo
-}

type MSet a = [(a,Int)]

--a) Defina a fun¸c˜ao cardMSet :: MSet a-> Int que calcula a cardinalidade de um multi-conjunto. Por exemplo, cardMSet [(’b’,4),(’a’,2),(’c’,1)] devolve 7.
cardMSet :: MSet a-> Int
cardMSet [] = 0
cardMSet ((p,n):t) = n + cardMSet t

--(b) Defina a fun¸c˜ao moda :: MSet a-> [a] quedevolve a lista dos elementos com maior n´umerode ocorrˆencias
moda :: MSet a-> [a]
moda [] = []
moda ((p,n):t) = p : modaAux n t 

modaAux :: Int -> MSet a-> [a]
modaAux n ((p,n1):t) | n == n1 = p : modaAux n t
                     | otherwise = modaAux n t
            
--(c) Defina a fun¸c˜ao converteMSet :: MSet a-> [a] que converte um multi-conjunto numalista. Por exemplo, converteMSet [(’b’,4),(’a’,2),(’c’,1)] devolve ‘‘bbbbaac’’
converteMSet :: MSet a-> [a]
converteMSet [] = []
converteMSet ((p,0):t) = converteMSet t
converteMSet ((p,n):t) = p : converteMSet ((p,n-1):t)

--(d) Definaafun¸c˜ao addNcopies :: Eq a => MSet a-> a-> Int-> MSet aquefazainser¸c˜ao
--de um dado n´umero de ocorrˆencias de um elemento no multi-conjunto, mantendo a ordena¸c˜ao
--por ordem decrescente da multiplicidade. N˜ao use uma fun¸c˜ao de ordena¸c˜ao.
addNcopies :: Eq a => MSet a -> a -> Int -> MSet a
addNcopies [] x n = [(x,n)]
addNcopies ((y,m):t) x n | x == y  = insert (x, m+n) t
                         | m >= n  = (y,m) : addNcopies t x n
                         | otherwise = (x,n) : (y,m) : t

insert :: (Eq a) => (a,Int) -> MSet a -> MSet a
insert p [] = [p]
insert (x,n) ((y,m):t) | n <= m    = (y,m) : insert (x,n) t
                       | otherwise = (x,n) : (y,m) : t

{-
. Considere o seguinte tipo de dados para representar subconjuntos de n´umeros reais (Doubles).
data SReais = AA Double Double | FF Double Double
| AF Double Double | FA Double Double
| Uniao SReais SReais
(AA x y) representa o intervalo aberto ]x,y[, (FF x y) representa o intervalo fechado [x,y], (AF x
y) representa ]x,y], (FA x y) representa [x,y[ e (Uniao a b) a uni˜ao de conjuntos.
-}

{-
(a) Defina a SReais como instˆancia da classe Show, de forma a que, por exemplo, a apresenta¸c˜ao
do termo Uniao (Uniao (AA 4.2 5.5) (AF 3.1 7.0)) (FF (-12.3) 30.0) seja ((]4.2,5.5[ U ]3.1,7.0]) U [-12.3,30.0])
-}

data SReais = AA Double Double | FF Double Double | AF Double Double | FA Double Double | Uniao SReais SReais

apresentacao :: SReais -> String
apresentacao (AA s s1) = "]" ++ show s ++ "," ++ show s1 ++ "["
apresentacao (FF s s1) = "[" ++ show s ++ "," ++ show s1 ++ "]"
apresentacao (AF s s1) = "]" ++ show s ++ "," ++ show s1 ++ "]"
apresentacao (FA s s1) = "[" ++ show s ++ "," ++ show s1 ++ "["
apresentacao (Uniao s s1) = "(" ++ apresentacao s ++ " U " ++ apresentacao s1 ++ ")"

{-
(b) Defina a fun¸c˜ao pertence :: Double-> SReais-> Bool que testa se um elemento pertence
a um conjunto.
-}
pertence :: Double-> SReais-> Bool
pertence d (AA s s1) = if d > s && d < s1 then True else False
pertence d (AF s s1) = if d > s && d <= s1 then True else False
pertence d (FA s s1) = if d >= s && d < s1 then True else False
pertence d (FF s s1) = if d >= s && d <= s1 then True else False
pertence d (Uniao s s1) = if pertence d s || pertence d s1 then True else False

{-
c) Defina a fun¸c˜ao tira :: Double-> SReais-> SReais que retira um elemento de um con
junto.
-}
tira :: Double-> SReais-> SReais
tira d (AA a b) | d <= a || d >= b = AA a b
                | otherwise = Uniao (AA a d) (AA d b)
tira d (AF a b) | d <= a || d > b = AA a b
                | d == b = AA a b
                | otherwise = Uniao (AA a d) (AF a b)
tira d (FA a b) | d < a || d >= b = FA a b
                | d == b = AA a b
                | otherwise = Uniao (FA a d) (AA a b)
tira d (FF a b) | d < a || d > b = AA a b
                | d == a = AF a b
                | d == b = FA a b
                | otherwise = Uniao (FA a d) (AF a b)                


--3. Considere o seguinte tipo para representar ´arvores irregulares (rose trees).
data RTree a = R a [RTree a]

{-
(a) Defina a fun¸c˜ao percorre :: [Int]-> RTree a-> Maybe [a] que recebe um caminho e
uma ´arvore e d´a a lista de valores por onde esse caminho passa. Se o caminho n˜ao for v´alido
a fun¸c˜ao deve retornar Nothing. O caminho ´e representado por uma lista de inteiros (1 indica
seguir pela primeira sub-´arvore, 2 pela segunda, etc)
-}
percorre :: [Int]-> RTree a-> Maybe [a]
percorre [] (R x _) = Just [x]
percorre (h:t) (R x l) | h <= 0 || h > length l = Nothing
                       | otherwise =