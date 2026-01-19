import Data.Char
import Data.List
{-
1. Defina a função substs :: Eq a => (a,a) -> [a] -> [a] que recebe uma par (x,y) e uma
lista l e substitui todas as ocorrências de x em l por y.
Por exemplo, subst (5,1) [6,5,2,1,5,7] == [6,1,2,1,1,7]
-}
substs :: Eq a => (a,a) -> [a] -> [a]
substs _ [] = []
substs (x,y) (h:t) | x == h = y : substs (x,y) t
                   | otherwise = h : substs (x,y) t

{-
2. Defina a função progressao  ::  [Int]  ->  Maybe  Int que verifica se uma dada uma lista
contém  os  primeiros  valores  de  uma  progressão  aritmética  de  inteiros  (na  qual  todos  os
valores consecutivos possuem uma diferença comum), devolvendo essa diferença. Por exemplo,
progressao [-3,0,3,6,9,12] == Just 3
-}
progressao  ::  [Int]  ->  Maybe  Int
progressao [] = Nothing
progressao [_] = Nothing
progressao (h:h2:t) = progressaoAux (abs(h2-h)) h2 t
        where progressaoAux  :: Int -> Int -> [Int] -> Maybe Int
              progressaoAux n _ [] = Just n
              progressaoAux n x (h:t) | abs(x-h) == n = progressaoAux n h t
                                      | otherwise = Nothing

{-
progressao :: [Int] -> Maybe Int
progressao [] = Nothing
progressao [_] = Nothing
progressao (x:h:t) = progressaoAc (h - x, h) t
    where progressaoAc :: (Int, Int) -> [Int] -> Maybe Int
          progressaoAc (d, _) [] = Just d
          progressaoAc (d, prev) (h:t) | h - prev == d = progressaoAc (d, h) t
                                       | otherwise = Nothing
-}

{-
3. Defina a função removeElems  ::  [Int]  ->  [a]  ->  [a] que recebe uma lista (não neces%
sariamente ordenada) de índices e uma lista e remove os elementos da lista nessas posições.
Por exemplo, a invocação removeElems [7,1,5,3] "Programacao" deve dar como resultado
"Pormcao"
-}
removeElems  ::  [Int]  ->  [a]  ->  [a]
removeElems x l = removeUmElem 0 si l
  where si = sort x

removeUmElem :: Int -> [Int] -> [a] -> [a]
removeUmElem _ [] as = as
removeUmElem _ _  [] = []
removeUmElem n (x:xs) (h:t)
    | n == x    = removeUmElem (n+1) xs t       -- remove posição x
    | otherwise = h : removeUmElem (n+1) (x:xs) t

--4. Considere as seguintes definições de tipos para representar uma tabela de abreviaturas que
--associa a cada abreviatura a palavra que ela representa.
{-
Defina a função associa  ::  TabAbrev  ->  [(Palavra,[Abreviatura])] que recebe uma
tabela de abreviaturas, e transforma a tabela de forma a que a cada palavra fica associada a lista
de todas as abreviaturas dessa palavra. Por exemplo, a invocação associa ex pode dar como
--ex = [("muito","mt"),("que","q"),("maior",">"),("que","k"),("muito","mto")]
resultado [("muito",["mt","mto"]), ("que",["q","k"]), ("maior",[">"])]
-}
type TabAbrev = [(Palavra,Abreviatura)]
type Palavra = String
type Abreviatura = String
--ex = [("muito","mt"),("que","q"),("maior",">"),("que","k"),("muito","mto")]

associa :: TabAbrev -> [(Palavra,[Abreviatura])]
associa [] = []
associa ((palav,abrev):t) = insere palav abrev (associa t)

insere :: Palavra -> Abreviatura -> [(Palavra,[Abreviatura])] -> [(Palavra,[Abreviatura])]
insere p a [] = [(p,[a])]
insere p a ((palav,abrev):t) | p == palav = (palav,(a:abrev)) : t
                             | otherwise = (palav,abrev) : insere p a t

{-
5. Apresente uma definição alternativa da função func, usando recursividade explícita em vez de
funções de ordem superior e fazendo uma única travessia da lista
-}
func :: Int -> [Int] -> [(Int,Int)]
func x l = filter ((>10) . snd)  (map (\y -> (x,y+x)) l) -- Se x for maior que 10 construi um par

func' :: Int -> [Int] -> [(Int,Int)]
func' n [] = []
func' n (h:t) | n + h > 10 = (n,(n+h)) : func' n t
              | otherwise = func' n t


--6. Considere o seguinte tipo para representar árvores binárias:
data BTree a = Empty | Node a (BTree a) (BTree a)

arv :: BTree Int
arv = Node 20 (Node 3 (Node 9 (Node 7 Empty Empty)
                              (Node 15 Empty Empty)) Empty)
              (Node 4 Empty (Node 3 Empty Empty))
{-
(a) Defina uma função isTrace :: Eq a => [a] -> BTree a -> Bool que testa se uma
dada lista é um traço de uma árvore (um traço é um caminho desde a raíz até uma das
subárvores vazias).
Por exemplo, isTrace [20,3,9,15] arv == True, mas isTrace [20,3] arv == False.
-}
isTrace :: Eq a => [a] -> BTree a -> Bool
isTrace [] _ = False
isTrace _ Empty = False
isTrace [h] (Node x Empty Empty) = h == x
isTrace (h:t) (Node x e d) = h == x && isTrace t e || h == x && isTrace t d 

{-
(b) Defina a função listaValores :: BTree a -> [[a]]  que produz a lista dos valores que
estão em cada nível da árvore.
Por exemplo, listaValores arv == [[20],[3,4],[9,3],[7,15]].
-}
listaValores :: BTree a -> [[a]]
listaValores Empty = []
listaValores (Node x e d) =
    [ [x] ] ++ junta (listaValores e) (listaValores d)

-- junta níveis da esquerda e direita
junta :: [[a]] -> [[a]] -> [[a]]
junta [] ys = ys
junta xs [] = xs
junta (x:xs) (y:ys) = (x ++ y) : junta xs ys
