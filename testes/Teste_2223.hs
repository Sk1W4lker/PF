{-
 1.Apresente uma definicao recursiva da fun¸c˜ao unlines :: [String]-> String que junta to
das as strings da lista numa s´o, separando-as pelo caracter’\n’.
 Por exemplo,unlines ["Prog", "Func"] == "Prog\nFunc".
-}
unlines' :: [String]-> String
unlines' [] = ""
unlines' [x] = x
unlines' (h:t) = h ++ "\n" ++ unlines' t

{-
2. O formato csv (comma separated values) serve para descrever tabelas de uma forma textual:
cada linha da tabela corresponde a uma linha do texto, enquanto que os elementos de cada linha
se encontram separados por v«ırgulas.
Por exemplo, a string "2,3,6,4\n12,3,12,4\n3,-4,5,7"
pode ser usada para descrever a matriz
-}

{-
(a) Considere o tipo type Mat = [[Int]] para representar matrizes e a seguinte defini ̧c ̃ao da
fun ̧c ̃ao stringToMat que converte strings desse formato em matrizes:
stringToMat :: String -> Mat
stringToMat s = map stringToVector (lines s)
-}
type Mat = [[Int]]

stringToMat :: String -> Mat
stringToMat s = map stringToVector (lines s) 

stringToVector :: String -> [Int]
stringToVector s = map read (separaVirg s)

separaVirg :: String -> [String] 
separaVirg [] = [""]
separaVirg (',':t) = "" : separaVirg t
separaVirg (h:t) =
    let (x:xs) = separaVirg t
    in  (h:x) : xs

{-
(b) Defina a fun ̧c ̃ao transposta :: String -> String que recebe a tabela em formato tex-
tual e devolve a tabela transposta tamb«em em formato textual.
-}
transp :: [[a]] -> [[a]]
transp ([]:_) = []
transp m = map head m : transp (map tail m)

linhaToString :: [Int] -> String
linhaToString [] = ""
linhaToString [x] = show x
linhaToString (h:t) = show h ++ "," ++ linhaToString t

matToString :: [[Int]] -> String
matToString [] = ""
matToString [l] = linhaToString l
matToString (h:t) = linhaToString h ++ "\n" ++ matToString t

transposta :: String -> String
transposta s = matToString (transp (stringToMat s))
{-
3. Considere o seguinte tipo de dados para representar uma lista em que os elementos podem ser
acrescentados ‘a esquerda (Esq) ou ‘a direita ( Dir) da lista. Nula representa a lista vazia.
-}
data Lista a = Esq a (Lista a) | Dir (Lista a) a | Nula

{-
(a) Defina a fun ̧c ̃ao semUltimo :: Lista a -> Lista a que recebe uma Lista n ̃ao vazia e
devolve a Lista sem o seu elemento mais ‘a direita.
-}
semUltimo :: Lista a -> Lista a
semUltimo (Esq a Nula) = Nula
semUltimo (Esq a e) = Esq a (semUltimo e)
semUltimo (Dir d a) = d --a é o ultimo elemento

{-
(b) Defina Lista como instˆancia da classe Show de forma a que a lista Esq 1 (Dir (Dir (Esq
9 Nula) 3) 4) seja apresentada como [1, 9, 3, 4]
-}
instance Show a => Show (Lista a) where
    show l = show(showLista l)

showLista :: Show a => Lista a -> [a]
showLista Nula = []
showLista (Esq a e) = a : showLista e
showLista (Dir d a) = showLista d ++ [a]

{-
4. Relembre a defini ̧c ̃ao do tipo das «arvores bin«arias e da fun ̧c ̃ao que faz uma travessia inorder de
uma «arvore.
data BTree a = Empty | Node a (BTree a) (BTree a)
inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node r e d) = (inorder e) ++ (r:inorder d)
-}

data BTree a = Empty | Node a (BTree a) (BTree a)

inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node r e d) = (inorder e) ++ (r:inorder d)

{-
(a) Defina uma fun ̧c ̃ao numera :: BTree a -> BTree (a,Int) que coloca em cada nodo da
«arvore argumento o n«umero de ordem desse nodo numa travessia inorder. A fun ̧c ̃ao deve
percorrer a «arvore uma «unica vez.
Por exemplo, numera (Node ’a’ (Node ’b’ Empty Empty) (Node ’c’ Empty Empty))
deve dar como resultado (Node (’a’,2) (Node (’b’,1) Empty Empty) (Node (’c’,3)
Empty Empty))
Sugest ̃ao: Comece por definir a fun ̧c ̃ao numeraAux :: Int -> BTree a -> (Int,BTree
(a,Int)) que recebe um inteiro (o primeiro n«umero a ser usado) e retorna a «arvore numerada
bem como o n«umero de elementos dessa «arvore.
-}
numeraAux :: Int -> BTree a -> (Int , BTree (a,Int))
numeraAux n Empty = (n, Empty)
numeraAux n (Node x l r) = (n2, Node (x,n1) l' r')
  where
    (n1, l') = numeraAux n l
    (n2, r') = numeraAux (n1+1) r

numera :: BTree a -> BTree (a,Int)
numera t = arv
  where
    (_, arv) = numeraAux 1 t

{-
(b) A fun ̧c ̃ao inorder n ̃ao «e injectiva: h«a muitas «arvores diferentes que d ̃ao origem ‘a mesma
travessia: por exemplo, as «arvores Node 1 Empty (Node 2 Empty Empty) e Node 2 (Node
1 Empty Empty) Empty tˆem como travessia a lista [1,2]
Defina a fun ̧c ̃ao unInorder :: [a] -> [BTree a] que, dada uma lista, calcula (a lista
de) todas as «arvores cuja travessia inorder corresponde a essa lista.
N«umero: Nome: Curso:
-}
unInorder :: [a] -> [BTree a]
unInorder [] = [Empty]
unInorder xs = 
  [ Node r l r'
  | i <- [0 .. length xs - 1]
  , let r = xs !! i
  , let esq = take i xs
  , let dir = drop (i+1) xs
  , l  <- unInorder esq
  , r' <- unInorder dir
  ]
