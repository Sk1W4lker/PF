{-
1. Considere express˜oes representadas como listas de n´umeros inteiros contˆem apenas multiplica¸c˜oes, adi¸c˜oes ou
subtra¸c˜oes que associam `a direita. Por exemplo, [5,3,8]— pode corresponder a 5 + (3 + 8), a 5 - (3 + 8), a
5 * (3 - 8), etc. Defina a fun¸c˜ao resultado :: [Int] -> Maybe String que recebe a lista e um valor e que
determina se ´e poss´ıvel, utilizando apenas multiplica¸c˜oes, adi¸c˜oes e subtra¸c˜oes, obter o valor fornecido a partir de
todos os n´umeros da lista.
Por exemplo, resultado [5,3,8] 55 deve produzir Just "5 * (3 + 8)", mas resultado [10,2] 6 deve pro-
duzir Nothing, pois com os valores apenas conseguimos obter os resultados 10 + 2 == 12, 10 - 2 == 10 e 10 +
2 == 20, todos diferentes de 6.
-}

-- resultado xs alvo
-- Tenta encontrar uma expressão construída a partir da lista xs
-- cujo valor seja igual ao alvo.
-- Se existir, devolve a expressão em String; caso contrário, Nothing.

resultado :: [Int] -> Int -> Maybe String
resultado xs alvo = procura (exprs xs)
  where -- procura percorre a lista de expressões
    procura :: [(Int, String)] -> Maybe String
    procura [] = Nothing
    procura ((v,e):rest)
      | v == alvo = Just e
      | otherwise = procura rest


-- Gera todas as expressões possíveis a partir da lista xs.
-- (valor da expressão, string da expressão)
exprs :: [Int] -> [(Int, String)]
exprs [x] = [(x, show x)]
exprs (x:xs) =
     --soma
     [ (x + v, show x ++ " + (" ++ e ++ ")") | (v,e) <- exprs xs]
  ++ -- subtração
     [ (x - v, show x ++ " - (" ++ e ++ ")")
     | (v,e) <- exprs xs
     ]
  ++ -- multiplicação
     [ (x * v, show x ++ " * (" ++ e ++ ")")
     | (v,e) <- exprs xs
     ]
{-
2. Considere a seguinte defini¸c˜ao da fun¸c˜ao deleteMin que remove todas as ocorrˆencias do menor valor de uma lista:
deleteMin :: Ord a => [a] -> [a]
deleteMin l = let m = minimum l
in filter (/= m) l

a) Relembre a t´ecnica de tupling e comece por definir uma fun¸c˜ao auxiliar minRem :: Ord a => [a] ->
(a,[a]) que calcula o m´ınimo de uma lista e a lista sem esse m´ınimo.
Use a fun¸c˜ao minRem para obter uma defini¸c˜ao alternativa da fun¸c˜ao deleteMin que percorre uma ´unica vez
a lista de entrada.
-}
-

{-
b) Use a fun¸c˜ao minRem para definir uma fun¸c˜ao que ordena uma lista removendo as repeti¸c˜oes de elementos.
-}
minOrdena :: Ord a => [a] -> [a]
minOrdena [] = []
minOrdena l = a : minOrdena (removeAll a b)
        where (a,b) = minRem l

--Se x já existir remove
removeAll :: Eq a => a -> [a] -> [a]
removeAll _ [] = []
removeAll x (h:t) | x == h = removeAll x t
                  | otherwise = h : removeAll x t

{-
3. Considere a seguinte representa¸c˜ao de ´arvores bin´arias em que os valores est˜ao nas suas folhas. `A direita mostra-se
ainda um exemplo de uma destas ´arvores.
data LTree a = Leaf a
| Fork (LTree a) (LTree a)
lt1 :: LTree Char
lt1 = Fork (Leaf ’A’)
(Fork (Fork (Leaf ’B’)
(Leaf ’C’))
(Leaf ’D’))

a) Cada folha de uma destas ´arvores pode ser identificada pelo caminho (path) que a liga `a ra´ız da ´arvore. Por
exemplo, na ´arvore lt1 acima, o caminho da raiz at´e `a folha ’C’ ´e Direita,Esquerda,Direita. Se em vez
disso representarmos estas decis˜oes como ’0’ e ’1’, esse caminho pode ser descrito pela String "101"
Defina a fun¸c˜ao selectLeaf :: String -> LTree a -> Maybe a que determina a folha de uma ´arvore
identificada por um dado caminho. Se esse caminho n˜ao conduzir a uma folha, a fun¸c˜ao deve retornar
Nothing.
-}
data LTree a = Leaf a | Fork (LTree a) (LTree a)

lt1 :: LTree Char
lt1 = Fork (Leaf 'A')
           (Fork (Fork (Leaf 'B')
                       (Leaf 'C'))
                 (Leaf 'D'))

selectLeaf :: String -> LTree a -> Maybe a
selectLeaf "" (Leaf x) = Just x
selectLeaf "" _        = Nothing
selectLeaf (h:t) (Fork e d) | h == '1' = selectLeaf t d
                            | h == '0' = selectLeaf t e
                            | otherwise = Nothing

{-
b) Considere agora a fun¸c˜ao apresentada ao lado,
que, dada uma ´arvore destas calcula a lista
de todas as suas folhas bem como o caminho
correspondente. Por exemplo, elemPath lt1 =
[("0",’A’),("100",’B’),("101",’C’),("11",’D’)]
elemPath :: LTree a -> [(String,a)]
elemPath (Leaf x) = [("",x)]
elemPath (Fork e d)
= [(’0’:p,x) | (p,x) <- elemPath e]
++ [(’1’:p,x) | (p,x) <- elemPath d]
Defina a fun¸c˜ao buildLTree :: [(String,a)] -> LTree a inversa da anterior, no sentido em que, para
qualquer ´arvore a se verifica que builLTree (elemPath a) == a. 
-}
elemPath :: LTree a -> [(String,a)]
elemPath (Leaf x) = [("",x)]
elemPath (Fork e d) = [('0':p,x) | (p,x) <- elemPath e] ++ [('1':p,x) | (p,x) <- elemPath d]

buildLTree :: [(String,a)] -> LTree a
buildLTree [("",x)] = Leaf x
buildLTree xs =
  Fork (buildLTree esq) (buildLTree dir)
  where
    esq = [(tail p, x) | (p,x) <- xs, head p == '0']
    dir = [(tail p, x) | (p,x) <- xs, head p == '1']
