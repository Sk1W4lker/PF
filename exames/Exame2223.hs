{-
1. Considere que se usa o tipo type MSet a = [(a,Int)] para representar multi-conjuntos de
elementos de a. Considere ainda que nestas listas n ̃ao h«a pares cuja primeira componente
coincida, nem cuja segunda componente seja menor ou igual a zero.
-}

type MSet a = [(a,Int)]
{-

(a) Defina a fun ̧c ̃ao converteMSet :: MSet a -> [a] que converte um multi-conjunto na
lista dos seus elementos Por exemplo, converteMSet [(’b’,2), (’a’,4), (’c’,1)]
corresponde a "bbaaaac".
-}

converteMSet :: MSet a -> [a]
converteMSet [] = []
converteMSet ((x,y):t) | y >= 1 = x : converteMSet ((x,(y-1)):t)
                       | otherwise = converteMSet t

{-
converteMSet :: MSet a -> [a] 
converteMSet l = concat $ map (\ (x,n) -> replicate n x) l
-}

{-
(b) Defina a fun ̧c ̃ao removeMSet :: Eq a => a -> MSet a -> MSet a que remove um ele-
mento a um multi-conjunto. Se o elemento n ̃ao existir, deve ser retornado o multi-conjunto
recebido.
Por exemplo, removeMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a [(’b’,2),
(’a’,4)], e removeMSet ’a’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a [(’b’,2),
(’a’,3), (’c’,1)].
-}
removeMSet :: Eq a => a -> MSet a -> MSet a
removeMSet n [] = []
removeMSet n ((x,y):t) | y > 1 && n == x = (x,(y-1)) : removeMSet n t
                       | y <= 1 && n == x = removeMSet n t
                       | otherwise = (x,y) : removeMSet n t

{-
(c) Defina a fun ̧c ̃ao uniaoMSet :: Eq a => MSet a -> MSet a -> MSet a que faz a uni ̃ao
de dois multi-conjuntos.
Por exemplo, uniaoMSet [(’b’,2),(’a’,4),(’c’,1)] [(’c’,7),(’a’,3),(’d’,5)] cor-
responde a [(’c’,8),(’a’,7),(’d’,5),(’b’,2)]
-}
add :: Eq a => (a,Int) -> MSet a -> MSet a
add (x,y) [] = [(x,y)]
add (x,y) ((x1,y1):t) | x == x1 = (x,y+y1) : t
                      | otherwise = (x1,y1) : add (x,y) t

uniaoMSet :: Eq a => MSet a -> MSet a -> MSet a
uniaoMSet xs [] = xs
uniaoMSet [] ys = ys
uniaoMSet (h:t) ys = uniaoMSet t (add h ys)

{-
2. Considere o seguinte tipo usado para descrever movimentos de um robot e a sua posi ̧c ̃ao numa
grelha.
type Posicao = (Int,Int)
data Movimento = Norte | Sul | Este | Oeste
data Caminho = C Posicao [Movimento]
Defina uma instˆancia da classe Eq para o tipo Caminho, considerando iguais os caminhos com
a mesma posi ̧c ̃ao de partida e de chegada e com o mesmo n«umero de movimentos.
-}
type Posicao = (Int,Int)
data Movimento = Norte | Sul | Este | Oeste
data Caminho = C Posicao [Movimento]

--Ele quer que o numero de movimentos seja igual ou seja usando length de ambos e comparando.
--Para pegar a posiçao de partida é só pegar o primeiro int dos par, a posiçao final é mais dificil
--tenho de fazer uma função


instance Eq Caminho where
    (==) = eqCaminho

finalPos :: Posicao -> [Movimento] -> Posicao
finalPos p [] = p
finalPos (x,y) (Norte:t) = finalPos (x,y+1) t
finalPos (x,y) (Sul:t)   = finalPos (x,y-1) t
finalPos (x,y) (Este:t)  = finalPos (x+1,y) t
finalPos (x,y) (Oeste:t) = finalPos (x-1,y) t

eqCaminho :: Caminho -> Caminho -> Bool
eqCaminho (C p1 mov1) (C p2 mov2) = p1 == p2 && finalPos p1 mov1 == finalPos p2 mov2 && length mov1 == length mov2

{-
3. Apresente uma defini ̧c ̃ao alternativa da fun ̧c ̃ao func, usando recursividade expl«ıcita em vez de
fun ̧c ̃oes de ordem superior e fazendo uma «unica travessia da lista.
-}
func :: [[Int]] -> [Int]
func l = concat (filter (\x -> sum x >10) l)

func' :: [[Int]] -> [Int]
func' [] = []
func' (h:t) | sum h > 10 = h ++ func' t
            | otherwise = func' t

{-
4. Considere o seguinte tipo para representar express ̃oes proposicionais:
data Prop = Var String | Not Prop | And Prop Prop | Or Prop Prop
p1 :: Prop
p1 = Not (Or (And (Not (Var "A")) (Var "B")) (Var "C"))
-}
data Prop = Var String | Not Prop | And Prop Prop | Or Prop Prop

{-
(a) Defina a fun ̧c ̃ao eval :: [(String,Bool)] -> Prop -> Bool que, dado o valor l«ogico
das vari«aveis proposicionais, calcula o valor l«ogico de uma express ̃ao proposicional
-}

--Procurar o valor
valorProc :: [(String,Bool)] -> String -> Bool
valorProc ((s,b):t) x | s == x = b
                      | otherwise = valorProc t x

eval :: [(String,Bool)] -> Prop -> Bool
eval a (Var x) = valorProc a x 
eval a (Not p) = not (eval a p)
eval a (And p1 p2) = (eval a p1) && (eval a p2)
eval a (Or p1 p2) = (eval a p1) || (eval a p2)

{-
b) Uma proposi ̧c ̃ao diz-se na forma normal negativa se as nega ̧c ̃oes s«o est ̃ao aplicadas ‘as
vari«aves proposicionais. Por exemplo, a proposi ̧c ̃ao ((A_ ́B)^ ́C) est«a na forma normal 
negativa, enquanto p1 n ̃ao est«a.
Defina a fun ̧c ̃ao nnf :: Prop -> Prop que recebe uma proposi ̧c ̃ao e produz uma outra
que lhe «e equivalente, mas que est«a na forma normal negativa. Por exemplo, o resultado
de nnf p1 dever«a ser a proposi ̧c ̃ao ((A_ ́B) ^ ́C).
Lembre-se das seguintes leis:  ́ not(not(A))= A,  not́(A or B) =  ́A^ ́Be  ́(A^B) =  ́A_ ́B
-}

nnf :: Prop -> Prop
nnf (Not p) =
    case p of
      Var x      -> Not (Var x)
      Not q      -> nnf q
      And x y    -> Or  (nnf (Not x)) (nnf (Not y))
      Or  x y    -> And (nnf (Not x)) (nnf (Not y))
nnf (And p q) = And (nnf p) (nnf q)
nnf (Or  p q) = Or  (nnf p) (nnf q)
nnf p = p
