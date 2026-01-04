module Ficha6 where
    
--1. Considere o seguinte tipo para representar  ́arvores bin ́arias.
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

{-
arvore = (Node 5 (Node 2 (Node 1 Empty
                                 Empty) 
                         (Node 3 Empty 
                                 Empty)) 
                 (Node 9 (Node 7 (Node 6 Empty 
                                         Empty) 
                                 (Node 8 Empty 
                                         Empty)) 
                         Empty))
-}

--(a) altura :: BTree a -> Int que calcula a altura da  ́arvore.
altura :: BTree a -> Int
altura Empty = 0
altura (Node _ e d) = 1 + max (altura e) (altura d)

--(b) contaNodos :: BTree a -> Int que calcula o n ́umero de nodos da  ́arvore.
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node _ e d) = 1 + contaNodos e + contaNodos d

--(c) folhas :: BTree a -> Int, que calcula o n ́umero de folhas (i.e., nodos sem descendentes) da  ́arvore.
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1 
folhas (Node _ e d) = folhas e + folhas d

--(d) prune :: Int -> BTree a -> BTree a, que remove de uma  ́arvore todos os elementos a partir de uma determinada profundidade.
prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune n (Node x e d) = Node x (prune (n-1) e) (prune (n-1) d)

--(e) path :: [Bool] -> BTree a -> [a], que dado um caminho (False corresponde
--a esquerda e True a direita) e uma  ́arvore, d ́a a lista com a informa ̧c ̃ao dos nodos
--por onde esse caminho passa.

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node e _ _) = [e]
path (False:t) (Node x e d) = x : path t e
path (True:t) (Node x e d) = x : path t d

--(f) mirror :: BTree a -> BTree a, que d ́a a  ́arvore sim ́etrica.
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node x e d) = Node x (mirror d) (mirror e)

--(g) zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c que generaliza a fun ̧c ̃ao zipWith para  ́arvores bin ́arias.
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ Empty _ = Empty
zipWithBT _ _ Empty = Empty
zipWithBT f (Node x e d) (Node x' e' d') = Node (f x x') (zipWithBT f e e') (zipWithBT f d d')

--(h) unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c), que generaliza a funcao unzip (neste caso de triplos) para  ́arvores bin ́arias.
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (a,b,c) e d) = (Node a unzipE1 unzipD1, Node b unzipE2 unzipD2, Node c unzipE3 unzipD3)
        where (unzipE1, unzipE2, unzipE3) = unzipBT e
              (unzipD1, unzipD2, unzipD3) = unzipBT d

--2. Defina as seguintes fun ̧c ̃oes, assumindo agora que as  ́arvores s ̃ao bin ́arias de procura:

--(a) Defina uma fun ̧c ̃ao minimo :: Ord a => BTree a -> a que calcula o menor elemento de uma  ́arvore bin ́aria de procura n ̃ao vazia.
-- # NOTA - Em arvores de procura (Node x e d) -> e < x < d
minimo :: Ord a => BTree a -> a
minimo (Node x Empty _) = x
minimo (Node x e d) = minimo e

--(b) Defina uma fun ̧c ̃ao semMinimo :: Ord a => BTree a -> BTree a que remove o menor elemento de uma  ́arvore bin ́aria de procura n ̃ao vazia.
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty d) = x
semMinimo (Node x e d) = Node x (semMinimo e) d

--(c) Defina uma fun ̧c ̃ao minSmin :: Ord a => BTree a -> (a,BTree a) que calcula, com uma  ́unica travessia da  ́arvore o resultado das duas fun ̧c ̃oes anteriores.
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty d) = (x,d)
minSmin (Node x e d) = (l1, Node x l2 d)
               where (l1,l2) = minSmin e

--(d) Defina uma fun ̧c ̃ao remove :: Ord a => a -> BTree a -> BTree a que remove um elemento de uma  ́arvore bin ́aria de procura, usando a fun ̧c ̃ao anterior.
remove :: Ord a => a -> BTree a -> BTree a 
remove n (Node x e d) 
    | n < x = Node x (remove n e) d 
    | n > x = Node x e (remove n d)
    | otherwise = case d of 
       Empty -> e
       _ -> Node v e t 
    where (v,t) = minSmin d 

--3. Considere agora que guardamos a informa ̧c ̃ao sobre uma turma de alunos na seguinte estrutura de dados:

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int | Rep | Faltou deriving Show
type Turma = BTree Aluno --  ́arvore bin ́aria de procura (ordenada por n ́umero

turmaEx :: Turma
turmaEx =
    Node (10,"Luís",ORD,Aprov 14)
        (Node (5,"Joana",MEL,Faltou)
            (Node (3,"Diogo",TE,Rep) Empty Empty)
            (Node (7,"Lara",ORD,Aprov 19) Empty Empty))
        (Node (15,"Pedro",TE,Aprov 10)
            Empty
            (Node (20,"Sofia",ORD,Aprov 20) Empty Empty))


--(a) inscNum :: Numero -> Turma -> Bool, que verifica se um aluno, com um dado numero, est ́a inscrito.
inscNum :: Numero -> Turma -> Bool 
inscNum _ Empty = False 
inscNum n (Node (num,_,_,_) e d) | num == n = True
                                 | n > num = inscNum n d 
                                 | otherwise = inscNum n e 

--(b) inscNome :: Nome -> Turma -> Bool, que verifica se um aluno, com um dado nome, est ́a inscrito.
inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome n (Node (_,name,_,_) e d) = n == name || inscNome n e || inscNome n d

--(c) trabEst :: Turma -> [(Numero,Nome)], que lista o n ́umero e nome dos alunos trabalhadores-estudantes (ordenados por n ́umero).
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nome,TE,_) e d) = trabEst e ++ [(num,nome)] ++ trabEst d
trabEst (Node (num,nome,_,_) e d) = trabEst e ++ trabEst d

--(d) nota :: Numero -> Turma -> Maybe Classificacao, que calcula a classifica ̧c ̃ao de um aluno (se o aluno n ̃ao estiver inscrito a fun ̧c ̃ao deve retornar Nothing)
nota :: Numero -> Turma -> Maybe Classificacao
nota n Empty = Nothing
nota n (Node (num,nome,_,cl) e d) | n == num = Just cl
                                  | n < num = nota n e
                                  | otherwise = nota n d

--(e) percFaltas :: Turma -> Float, que calcula a percentagem de alunos que faltaram á avaliacao.
percFaltas :: Turma -> Float
percFaltas arv = (nFaltas arv / fromIntegral (contaNodos arv)) * 100


nFaltas :: Turma -> Float
nFaltas Empty = 0
nFaltas (Node (_,_,_,Faltou) e d) = 1 + nFaltas e + nFaltas d
nFaltas (Node (_,_,_,_) e d) = nFaltas e + nFaltas d

--(f) mediaAprov :: Turma -> Float, que calcula a m ́edia das notas dos alunos que passaram.
mediaAprov :: Turma -> Float
mediaAprov arv = (pAlunos arv / nAprov arv) 

pAlunos :: Turma -> Float
pAlunos Empty = 0
pAlunos (Node (_,_,_,(Aprov n)) e d) = (fromIntegral n) + pAlunos e + pAlunos d
pAlunos (Node (_,_,_,_) e d) = pAlunos e + pAlunos d

nAprov :: Turma -> Float
nAprov Empty = 0
nAprov (Node (_,_,_,Aprov _) e d) = 1 + nAprov e + nAprov d
nAprov (Node _ e d) = nAprov e + nAprov d

--(g) aprovAv :: Turma -> Float, que calcula o r ́acio de alunos aprovados por avaliados. Implemente esta fun ̧c ̃ao fazendo apenas uma travessia da  ́arvore 2

aprovAv :: Turma -> Float
aprovAv arv = (nAprov arv / avAlunos arv) 

avAlunos :: Turma -> Float
avAlunos Empty = 0
avAlunos (Node (_,_,_,Faltou) e d) = avAlunos e + avAlunos d
avAlunos (Node (_,_,_,_) e d) = 1 + avAlunos e + avAlunos d
