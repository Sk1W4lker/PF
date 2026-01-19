import System.Random (randomRIO)

{-
1. Apresente uma defini ̧c ̃ao recursiva da fun ̧c ̃ao (pr«e-definida) zip :: [a] -> [b] -> [(a,b)]
constr«oi uma lista de pares a partir de duas listas. Por exemplo, zip [1,2,3] [10,20,30,40]
corresponde a [(1,10),(2,20),(3,30)].
-}
zip' :: [a] -> [b] -> [(a,b)]
zip' x [] = []
zip' [] y = []
zip' (h:t) (h1:t1) = (h,h1) : zip' t t1

{-
2. Defina a fun ̧c ̃ao preCrescente :: Ord a => [a] -> [a] que calcula o maior prefixo cres-
cente de uma lista. Por exemplo, preCrescente [3,7,9,6,10,22] corresponde a [3,7,9] e
preCrescente [1,2,7,9,9,1,8] corresponde a [1,2,7,9].
-}
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (x:y:z) | x < y = x : preCrescente (y:z)
                     | otherwise = [x] 

{-
3. A amplitude de uma lista de inteiros define-se como a diferen ̧ca entre o maior e o menor dos
elementos da lista (a amplitude de uma lista vazia «e 0). Defina a fun ̧c ̃ao amplitude :: [Int]
-> Int que calcula a amplitude de uma lista (idealmente numa «unica passagem pela lista).
-}
amplitude :: [Int] -> Int
amplitude [] = 0
amplitude (h:t) = maximo - minimo
  where
    (maximo,minimo) = aux h h t

aux :: Int -> Int -> [Int] -> (Int,Int)
aux maximo minimo [] = (maximo,minimo)
aux maximo minimo (h:t) = aux (max maximo h) (min minimo h) t

{-
4. Considere o sequinte tipo type Mat a = [[a]] para representar matrizes. Defina a fun ̧c ̃ao soma
:: Num a => Mat a -> Mat a -> Mat a que soma duas matrizes da mesma dimens ̃ao.
-}
type Mat a = [[a]]

soma :: Num a => Mat a -> Mat a -> Mat a
soma n p = zipWith(zipWith(+)) n p

{-
5. Decidiu-se organizar uma agenda telef«onica numa «arvore bin«aria de procura (ordenada por ordem
alfab«etica de nomes). Para isso, declararam-se os seguintes tipos de dados:
type Nome = String
type Telefone = Integer
data Agenda = Vazia | Nodo (Nome,[Telefone]) Agenda Agenda
Defina Agenda como instˆancia da classe Show de forma a que a visualiza ̧c ̃ao da «arvore resulte
numa listagem da informa ̧c ̃ao ordenada por ordem alfab«etica (com um registo por linha) e em
que os v«arios telefones associados a um nome se apresentem separados por / .
-}
type Nome = String
type Telefone = Integer
data Agenda = Vazia | Nodo (Nome,[Telefone]) Agenda Agenda

instance Show Agenda where
    show = showAgenda


showTelefone :: [Telefone] -> String
showTelefone [] = ""
showTelefone [x] = show x
showTelefone (h:t) = show h ++ "/" ++ showTelefone t

showAgenda :: Agenda -> String
showAgenda Vazia = ""
showAgenda (Nodo (n,t) e d) = showAgenda e ++ n ++ " " ++ showTelefone t ++ "\n" ++ showAgenda d

{-
6. Defina uma fun ̧c ̃ao randomSel :: Int -> [a] -> IO [a] que dado um inteiro n e uma lista 
l, produz uma lista com n elementos seleccionados aleatoriamente de l. Um elemento n ̃ao pode
aparecer na lista produzida mais vezes do que a parece na lista argumento. Se n for maior do
que o comprimento da lista a fun ̧c ̃ao dever«a retornar uma permuta ̧c ̃ao da lista argumento. Por
exemplo, a invoca ̧c ̃ao de randomSel 3 [1,3,1,4,2,8,9,5]] poderia produzir qualquer uma
das listas [1,4,2], [5,2,8] ou [1,9,1], mas nunca [2,3,2]
-}
randomSel :: Int -> [a] -> IO [a]
randomSel _ [] = return []
randomSel 0 _  = return []
randomSel n xs = do
    let len = length xs
    i <- randomRIO (0, len-1)
    let (before, x:after) = splitAt i xs
    rest <- randomSel (n-1) (before ++ after)
    return (x : rest)

{-
7. Defina uma fun ̧c ̃ao organiza :: Eq a => [a] -> [(a,[Int])] que, dada uma lista constr«oi
uma lista em que, para cada elemento da lista original se guarda a lista dos «ındices onde esse el-
emento ocorre. Por exemplo, organiza "abracadabra" corresponde a [(’a’,[0,3,5,7,10]),
(’b’,[1,8]), (’r’,[2,9]),(c,[4])]
-}
organiza :: Eq a => [a] -> [(a,[Int])]
organiza xs = aux 0 xs
  where
    aux _ [] = []
    aux n (h:t) = insere h n (aux (n+1) t)

    insere :: Eq a => a -> Int -> [(a,[Int])] -> [(a,[Int])]
    insere x i [] = [(x,[i])]
    insere x i ((y,is):rest)
        | x == y    = (y,is ++ [i]) : rest
        | otherwise = (y,is) : insere x i rest

func :: [[Int]] -> [Int]
func l = concat (filter (\x -> sum x >10) l)

func' :: [[Int]] -> [Int]
func' [] = []
func' (h:t) | sum h > 10 = h ++ func' t 
            | otherwise = func t 

{-
9. Considere a seguinte estrutura para manter
um dicion«ario, onde as palavras est ̃ao orga-
nizadas de forma alfab«etica.
data RTree a = R a [RTree a]
type Dictionary = [ RTree (Char, Maybe String) ]
Cada «arvore agrupa todas as palavras
come ̧cadas numa dada letra. As palavras
constroem-se descendo na «arvore a partir
da raiz. Quando uma palavra est«a com-
pleta, o valor associado ‘a «ultima letra «e
Just s, sendo s uma string com a descri ̧c ̃ao
da palavra em causa (que corresponde ao
caminho desde a raiz at«e a«ı). Caso contr«ario
«e Nothing.
Por exemplo, d1 «e um dicion«ario com as
palavras: cara, caras, caroe carro.
d1 = [R (’c’,Nothing) [
R (’a’,Nothing) [ 
R (’r’,Nothing) [ 
R (’a’,Just "...") [ 
R (’s’,Just "...") [] ], 
R (’o’,Just "...") [], 
R (’r’,Nothing) [ 
R (’o’,Just "...") [] ] 
] ] ] ] 
Defina a fun ̧c ̃ao insere :: String -> String -> Dictionary -> Dictionary que, dadas
uma palavra e a informa ̧c ̃ao a ela associada, acrescenta essa entrada no dicion«ario. Se a palavra
j«a existir no dicion«ario, atualiza a informa ̧c ̃ao a ela associada.
-}
data RTree a = R a [RTree a]
type Dictionary = [ RTree (Char, Maybe String) ]
--i aint doing allat in a test

insere :: String -> String -> Dictionary -> Dictionary
insere [] _ d = d 
insere [x] i [] = [insere' [x] i]
insere [x] i ((R (c,f) l):ys) 
    | x < c = (R (x,Just i) []) : (R (c,f) l) : ys 
    | x == c = (R (c,Just i) l) : ys 
    | otherwise = if null ys 
                  then (R (c,f) l) : [R (x,Just i) []]
                  else (R (c,f) l) : insere [x] i ys 
insere (h:t) i ((R (c,f) l):r) 
    | h < c = insere' (h:t) i : (R (c,f) l) : r 
    | h == c = (R (c,f) (insere t i l)) : r 
    | otherwise = if null r
                  then (R (c,f) l) : [insere' (h:t) i] 
                  else (R (c,f) l) : insere (h:t) i r 

-- Funçaõ que cria uma RTree com a palavra e a informação indicada 
insere' :: String -> String -> RTree (Char,Maybe String)
insere' [x] i = R (x,Just i) []
insere' (h:t) i = R (h,Nothing) [insere' t i]