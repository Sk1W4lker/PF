module Ficha7 where

--1. Considere o seguinte tipo para representar express ̃oes inteiras.
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

conta :: ExpInt 
conta = Mais 
          (Menos 
                (Const 7) 
                (Const 4))
          (Mult 
                (Simetrico (Const 2)) 
                (Mult 
                     (Const 5) 
                     (Const 5)))

--(a) Defina uma fun ̧c ̃ao calcula :: ExpInt -> Int que, dada uma destas express ̃oes calcula o seu valor.
calcula :: ExpInt -> Int
calcula (Const n) = n
calcula (Simetrico n) = - calcula n
calcula (Mais x y) = calcula x + calcula y
calcula (Menos x y) = calcula x - calcula y
calcula (Mult x y) = calcula x * calcula y

{-
(b) Defina uma fun ̧c ̃ao infixa :: ExpInt -> String de forma a que
infixa (Mais (Const 3) (Menos (Const 2) (Const 5))) dˆe como resultado
"(3 + (2 - 5))".
-}
infixa :: ExpInt -> String
infixa (Const n) = show n
infixa (Simetrico n) =  "(-" ++ infixa n ++ ")"
infixa (Mais x y) = "(" ++ infixa x ++ " + " ++ infixa y ++ ")"
infixa (Menos x y) = "(" ++ infixa x ++ " - " ++ infixa y ++ ")"
infixa (Mult x y) = "(" ++ infixa x ++ " x " ++ infixa y ++ ")"

--(c) Defina uma outra fun ̧c ̃ao de convers ̃ao para strings posfixa :: ExpInt -> String
--de forma a que quando aplicada `a express ̃ao acima dˆe como resultado "3 2 5 -+".
posfixa :: ExpInt -> String
posfixa (Const n) = show n
posfixa (Simetrico n) = posfixa n ++ " (-)"
posfixa (Mais x y) = posfixa x ++ " " ++ posfixa y ++ "+"
posfixa (Menos x y) = posfixa x ++ " " ++ posfixa y ++  "-"
posfixa (Mult x y) = posfixa x ++ " " ++ posfixa y ++ "x"


--2) Considere o seguinte tipo para representar  ́arvores irregulares (rose trees ).
data RTree a = R a [RTree a] deriving Show

arv :: RTree Int 
arv = R 3 [ 
            R 5 [ R 1 []],
            R 3 [],
            R 2 [
                  R 6 [R 5 []],
                  R 3 [],
                  R 1 []
                ],
            R 7 [ R 2 [R 1 []]]
          ]

--(a) soma :: Num a => RTree a -> a que soma os elementos da  ́arvore.
soma :: Num a => RTree a -> a
soma (R x []) = x
soma (R x l) = x + sum(map soma l)

--(b) altura :: RTree a -> Int que calcula a altura da  ́arvore.
altura :: RTree a -> Int
altura (R x []) = 1
altura (R x l) = 1 + maximum(map altura l) --max para n, maximum para listas

--(c) prune :: Int -> RTree a -> RTree a que remove de uma  ́arvore todos os elementos a partir de uma determinada profundidade
prune :: Int -> RTree a -> RTree a
prune 0 (R x _) = R x []
prune n (R x l) = (R x (map (prune (n-1)) l))

--(d) mirror :: RTree a -> RTree a que gera a  ́arvore sim ́etrica.
mirror :: RTree a -> RTree a
mirror (R x l) = R x (map mirror(reverse l))

--(e) postorder :: RTree a -> [a] que corresponde `a travessia postorder da  ́arvore.
--e)                            esquerda -> direita -> topo              
postorder :: RTree a -> [a]
postorder (R x []) = [x]
postorder (R x l) = concat (map postorder l) ++ [x]

--                              topo -> esquerda -> direita
preorder :: RTree a -> [a]
preorder (R x []) = [x]
preorder (R x l) = x : concat (map preorder l)

{-
3. Relembre a defini ̧c ̃ao de  ́arvores bin ́arias apresentada na ficha anterior:
data BTree a = Empty | Node a (BTree a) (BTree a)
Nestas  ́arvores a informa ̧c ̃ao est ́a nos nodos (as extermidades da  ́arvore tˆem apenas
uma marca – Empty).  ́E tamb ́em habitual definirem-se  ́arvores em que a informa ̧c ̃ao
est ́a apenas nas extermidades (leaf trees ):
-}
data LTree a = Tip a | Fork (LTree a) (LTree a)

--Defina sobre este tipo as seguintes fun ̧c ̃oes

arv1 :: LTree Int 
arv1 = Fork 
           (Fork (Tip 3) 
                 (Fork (Tip 2) (Tip 1))
           )
           (Fork (Fork 
                       (Fork (Tip 4) (Tip 2))
                       (Tip 5)
                 )
                 (Fork (Tip 1) (Tip 3))
           )

--(a) ltSum :: Num a => LTree a -> a que soma as folhas de uma  ́arvore
ltSum :: Num a => LTree a -> a
ltSum (Tip n) = n
ltSum (Fork x y) = ltSum x + ltSum y

--(b) listaLT :: LTree a -> [a] que lista as folhas de uma  ́arvore (da esquerda para a direita).
listaLT :: LTree a -> [a]
listaLT (Tip n) = [n]
listaLT (Fork x y) = listaLT x ++ listaLT y

--(c) ltHeight :: LTree a -> Int que calcula a altura de uma  ́arvore.
ltHeight :: LTree a -> Int
ltHeight (Tip _) = 0
ltHeight (Fork x y) = 1 + max (ltHeight x) (ltHeight y)

{-
4. Estes dois conceitos podem ser agrupados num s ́o, definindo o seguinte tipo:
data FTree a b = Leaf b | No a (FTree a b) (FTree a b)
S ̃ao as chamadas full trees onde a informa ̧c ̃ao est ́a n ̃ao s ́o nos nodos, como tamb ́em nas
folhas (note que o tipo da informa ̧c ̃ao nos nodos e nas folhas n ̃ao tem que ser o mesmo).
-}
data BTree a = Empty | Node a (BTree a) (BTree a)
    deriving Show

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

arv11 :: FTree Int Char 
arv11 = No 3 
          (No 4 
               (Leaf 'g') 
               (Leaf 'b')
          )
          (No 1 
               (No 5 (Leaf 'd') (Leaf 'a')) 
               (Leaf 'k')
          )
{-
data LTree a = Tip a | Fork (LTree a) (LTree a)

data BTree a = Empty | Node a (BTree a) (BTree a)
    deriving Show

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)
-}


--a) que separa uma  ́arvore com informa ̧c ̃ao nos nodos e nas folhas em duas  ́arvores de tipos diferentes
splitFTree :: FTree a b -> (BTree a, LTree b)
-- Caso folha: não há informação na BTree, mas há na LTree
splitFTree (Leaf b) = (Empty, Tip b)
-- Caso nó interno: separar recursivamente a esquerda e a direita
splitFTree (No x left right) = (Node x bLeft bRight,Fork lLeft lRight)      
    where (bLeft,  lLeft ) = splitFTree left   -- separar subárvore esquerda
          (bRight, lRight) = splitFTree right  -- separar subárvore direita

--Defina ainda a fun ̧c ̃ao joinTrees :: BTree a -> LTree b -> Maybe (FTree a b) que sempre que as  ́arvores sejam compat ́ıveis as junta numa s ́o
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip x) = Just (Leaf x)
joinTrees (Node z e d) (Fork a b) =    
    case (joinTrees e a, joinTrees d b) of 
        (Just x, Just y) -> Just (No z x y)
        _ -> Nothing
joinTrees _ _ = Nothing