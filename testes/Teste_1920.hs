--1)
--a)
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (h:t) l | elem h l = h : intersect t l
                  | otherwise = intersect t l

--b)
tails :: [a] -> [[a]]
tails [] = [[]]
tails a@(h:t) = a : tails t

--2)
type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

--a)
elems :: ConjInt -> [Int]
elems [] = []
elems ((x,y):t) | y >= x = x : elems((x+1,y):t)
                | otherwise = elems t

--b)
geraConj :: [Int] -> ConjInt
geraConj [] = []
geraConj (h:t) = geraConjAux h h t

geraConjAux :: Int -> Int -> [Int] -> ConjInt
geraConjAux inicio fim [] = [(inicio,fim)]
geraConjAux inicio fim (h:t) | h == (fim+1) = geraConjAux inicio h t
                             | otherwise = (inicio,fim) : geraConjAux h h t

--3)
data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String deriving (Show)
type Nome = String
type Agenda = [(Nome, [Contacto])]
--a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email [] = [(nome, [Email email])]
acrescEmail nome email ((n,l):t) | nome == n = (n, Email email : l) : t
                                  | otherwise = (n, l) : acrescEmail nome email t

--b)
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [] = Nothing
verEmails nome ((n,l):t) | nome == n = Just (soEmails l)
                         | otherwise = verEmails nome t

soEmails :: [Contacto] -> [String]
soEmails [] = []
soEmails (Email l : t) = l : soEmails t
soEmails (_:t) = soEmails t

--c)
consulta :: [Contacto] -> ([Integer],[String]) 
consulta [] = ([],[])
consulta (s:t) = case s of
    Email e -> (a,e:b)
    Trab n -> (n:a,b)
    Casa n -> (n:a,b)
    Tlm n -> (n:a,b)
    where (a,b) = consulta t

--4)
--a)
data RTree a = R a [RTree a] deriving (Show, Eq)

paths :: RTree a -> [[a]]
paths (R x []) = [[x]]
paths (R x l) = map (x:) (concat (map paths l))

--b)
unpaths :: Eq a => [[a]] -> RTree a  
unpaths ([x]:_) = R x [] 
unpaths l = R ini( map unpaths (aux sec) )
    where ini = head(head l)
          sec = map (drop 1) l 

-- Função que separa as listas que têm a cabeça igual
aux :: Eq a => [[a]] -> [[[a]]] 
aux [] = []
aux m = agrupa m : aux (drop c m)
    where c = length(agrupa m) 

-- Função que recolhe as listas consecutivas com a mesma cabeça
agrupa :: Eq a => [[a]] -> [[a]]
agrupa [] = []
agrupa [x] = [x]
agrupa (h:s:t) | head s == head h = h : agrupa (s:t)
               | otherwise = [h]