--1)
--a)
subst :: Eq a => (a,a) -> [a] -> [a]
subst _ [] = []
subst (x,y) (h:t) | x == h = y : subst (x,y) t
                  | otherwise = h : subst (x,y) t

--b)
posicoes :: [a] -> [Int] -> [a]
posicoes _ [] = []
posicoes [] _ = []
posicoes l (h:t) = (l !! (h-1)) : posicoes l t

--2)

data BTree a b = Leaf b | Node a (BTree a b) (BTree a b)

--a) As folhas da  ́arvore (da esquerda para a direita).
folhas :: BTree a b -> [b]
folhas (Leaf x) = [x]
folhas (Node x e d) = folhas e ++ folhas d

--b)calcula os somatórios de todos os nodos interm ́edios e de todas as folhas da ́arvore.
somas :: BTree Float Int -> (Float,Int)
somas (Leaf x) = (0,x)
somas (Node x e d) = (x + le + fd, fe + ld)
    where (le,fe) = somas e
          (fd,ld) = somas d

--3. Considere a sequinte defini ̧c ̃ao para representar matrizes: type Mat a = [[a]].
type Mat a = [[a]]

transpose :: Mat a -> Mat a
transpose [] = []
transpose l = (map head l) : transpose (map tail l)

rotateLeft :: Mat a -> Mat a
rotateLeft [] = []
rotateLeft l = rotateLeft(map reverse l)

--4. Considere as seguintes defini ̧c ̃oes de tipos de dados para representar filmes:
type Filme = (Titulo,Realizador,[Actor],Genero,Ano)
type Titulo = String
type Realizador = String
type Actor = String
type Ano = Int

data Genero = Comedia | Drama | Ficcao | Accao | Animacao | Documentario deriving Eq
type Filmes = [Filme]

--a)
doRealizador :: Filmes -> Realizador -> [Titulo]
doRealizador [] _ = []
doRealizador ((title,real,_,_,_):t) n | n == real = title : doRealizador t n
                                    | otherwise = doRealizador t n
 
--b)
pertence :: Eq a => a -> [a] -> Bool
pertence _ [] = False
pertence x (h:t)
  | x == h    = True
  | otherwise = pertence x t

doActor :: Filmes -> Actor -> [Titulo]
doActor [] _ = []
doActor ((titulo, _, atores, _, _) : t) a
  | pertence a atores = titulo : doActor t a
  | otherwise         = doActor t a

--c)
consulta :: Filmes -> Genero -> Realizador -> [(Ano, Titulo)]
consulta bd gen rea = map aux (filter (teste gen rea) bd)
  where
    teste :: Genero -> Realizador -> Filme -> Bool
    teste g r (_,x,_,y,_) = g == y && r == x

    aux :: Filme -> (Ano, Titulo)
    aux (t, _, _, _, a) = (a, t)

--3)
data Avaliacao = NaoVi | Pontos Int-- pontua¸c~ao entre 1 e 5
type FilmesAval = [(Filme,[Avaliacao])]

--a)
avalia :: FilmesAval -> IO FilmesAval
avalia bd = do
    putStrLn "Titulo do filme:"
    tit <- getLine

    putStrLn "Avaliacao (1 a 5):"
    s <- getLine
    let n = read s :: Int

    let av = if n >= 1 && n <= 5 then Pontos n else NaoVi

    return (atualiza tit av bd)

atualiza :: Titulo -> Avaliacao -> FilmesAval -> FilmesAval
atualiza _ _ [] = []
atualiza tit av ((f@(t,_,_,_,_),avs):xs)
    | tit == t  = (f, av:avs) : xs
    | otherwise = (f, avs) : atualiza tit av xs
