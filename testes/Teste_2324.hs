{-
1. Defina a fun ̧c ̃ao alterna :: Num a => Int -> a -> [a] que dado um n«umero n e um 
valor v, constro«ı uma lista com n elementos alternadamente v e -v. Por exemplo, alterna 5
2 corresponde a [2,-2,2,-2,2].
-}

alterna :: Num a => Int -> a -> [a]
alterna n a | n <= 0 = []
            | otherwise = a : alterna (n-1) (-a)

data Turma = Empty | Node (Integer,String) Turma Turma

turmaEx2 :: Turma
turmaEx2 =
    Node (15,"Duarte")
        (Node (8,"Eva")
            (Node (3,"Filipe") Empty Empty)
            Empty
        )
        (Node (22,"Goncalo")
            (Node (17,"Helena") Empty Empty)
            Empty
        )

{-
(a) Declare Turma como instˆancia da classe Show de forma a que a visualiza ̧c ̃ao da turma seja
uma listagem da turma por ordem crescente de n«umero de aluno, com um registo por
linha
-}

showTurma :: Turma -> String
showTurma Empty = ""
showTurma (Node (num,nom) e d) = showTurma e ++ "(" ++ show num ++ ", " ++ nom ++ ")\n" ++ showTurma d

instance Show Turma where
    show = showTurma

--(b) Defina a fun ̧c ̃ao limites :: Turma -> (Integer,Integer) que d«a o par com o menor e o maior n«umero da turma.
limites :: Turma -> (Integer,Integer)
limites Empty = (0,0)
limites f = (minimo f, maximo f)

maximo :: Turma -> Integer
maximo Empty = 0
maximo (Node (num,nom) _ Empty) = num
maximo (Node _ _ d) = maximo d

minimo :: Turma -> Integer
minimo Empty = 0
minimo (Node (num,nom) Empty _) = num
minimo (Node _ e _) = minimo e

{-
3. Considere as seguintes defini ̧c ̃oes de tipos para representar uma tabela de abreviaturas que
associa a cada palavra uma abreviatura.
-}

type TabAbrev = [(Palavra,Abreviatura)]
type Palavra = String
type Abreviatura = String

{-
(a) Defina a fun ̧c ̃ao difMaior :: TabAbrev -> (Palavra,Int) que recebe um tabela n ̃ao
vazia, e descobre a palavra da tabela que mais se simplifica (isto «e, cuja diferen ̧ca entre o
n«umero de caracteres da palavra e da abreviatura «e maior). A fun ̧c ̃ao devolve o par com
a palavra e o n«umero de caracteres que foi reduzio.
Por exemplo, difMaior [("muito","mt"), ("que","q")] == ("muito",3).-}

{-
difMaior :: TabAbrev -> (Palavra,Int)
difMaior [(p,a)] = (p, length p - length a)
difMaior ((p1,a1):t)
    | (length p1 - length a1) >= y = (p1, length p1 - length a1)
    | otherwise                    = (x,y)
    where (x,y) = difMaior t
-}

difMaior :: TabAbrev -> (Palavra,Int)
difMaior (h:t) = difAux h t

difAux :: (Palavra,Abreviatura) -> [(Palavra,Abreviatura)] -> (Palavra,Int)
difAux (p,a) [] = (p,(length p - length a))
difAux (p,a) ((p1,a1):t) | (length p - length a) >= (length p1 - length a1) = difAux (p,a) t
                         | otherwise = difAux (p1,a1) t

difMaior' :: TabAbrev -> (Palavra,Int)
difMaior' a = difMaiorAux' ("",0) a 

difMaiorAux' :: (Palavra,Int) -> TabAbrev -> (Palavra,Int)
difMaiorAux' n [] = n
difMaiorAux' (a,n) ((palav,abrev):t) | n < diff = difMaiorAux' (palav,diff) t
                                     | otherwise = difMaiorAux' (a,n) t
                                     where diff = ((length palav) - (length abrev))

{-
(b) Defina a fun ̧c ̃ao subst :: [String] -> TabAbrev -> [String] que recebe um texto
(dado como uma lista de strings) e uma tabela de abreviaturas, substitui todas as abre-
viaturas que apare ̧cam no texto pelas respectivas palavras associadas.
-}
subst :: [String] -> TabAbrev -> [String]
subst [] _ = []
subst n [] = n
subst (h:t) ((palav,abrev):t1) | h == abrev = palav : subst t t1
                               | otherwise = subst t t1

{-
4. Considere a seguinte defini ̧c ̃ao da fun ̧c ̃ao dumpLT que, dada uma «arvore de folhas, constr«oi a
lista dos seus elementos anotados com o n«ıvel em que aparecem na «arvore.
Por exemplo, dumpLT (Fork (Tip ’a’) (Fork (Tip ’b’) (Tip ’c’))) corresponde ‘a lista
[(’a’,2), (’b’,3), (’c’,3)]
-}

data LTree a = Tip a | Fork (LTree a) (LTree a)

dumpLT :: LTree a -> [(a,Int)]
dumpLT (Tip x) = [(x,1)]
dumpLT (Fork e d) = map f (dumpLT e ++ dumpLT d)
    where f :: (a,Int) -> (a,Int)
          f (x,y) = (x,y+1)

{-
(a) Apresente uma defini ̧c ̃ao alternativa, mais eficiente e usando um parˆametro que corre-
sponde ao n«ıvel da «arvore.
-}
dumpLT' :: LTree a -> [(a,Int)]
dumpLT' tree = dumpLTAux tree 1

dumpLTAux :: LTree a -> Int -> [(a, Int)]
dumpLTAux (Tip x) n = [(x, n)]
dumpLTAux (Fork e d) n = dumpLTAux e (n+1) ++ dumpLTAux d (n+1)

{-
(b) Defina a fun ̧c ̃ao unDumpLT :: [(a,Int)] -> LTree a, inversa da anterior, no sentido
em que, para toda a «arvore lt se verifica que unDumpLT (dumpLT lt) == lt.
N«umero: Nome: Curso:
3
-}
unDumpLT :: [(a,Int)] -> LTree a
unDumpLT l = fst (buildTree 1 l)

buildTree :: Int -> [(a,Int)] -> (LTree a, [(a,Int)])
buildTree _ [] = error "Lista vazia"
buildTree level ((x,n):xs)
    | n == level = (Tip x, xs)
    | n > level  =
        let (left,  rest1) = buildTree (level + 1) ((x,n):xs)
            (right, rest2) = buildTree (level + 1) rest1
        in (Fork left right, rest2)
    | otherwise  = error "Nível inválido"