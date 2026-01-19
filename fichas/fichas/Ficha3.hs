module Ficha3 where

data Hora = H Int Int
            deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

---------------------------------------
---------Auxiliares da Ficha 1---------
---------------------------------------

--a) testar se um par de inteiros representa uma hora do dia v ́alida;
horaValida :: Hora -> Bool
horaValida (H p1 p2) = p1 >= 0 && p1 <= 23 && p2 >= 0 && p2 <= 59

--b) testar se uma hora  ́e ou n ̃ao depois de outra (compara ̧c ̃ao);
horaTempoDeChegadaSuperior :: Hora -> Hora -> Bool
horaTempoDeChegadaSuperior (H p1 p2) (H p3 p4) = p1 == p3 && p4 > p2 || p3 > p1 

--c) converter um valor em horas (par de inteiros) para minutos (inteiro);
converteHoras :: Hora -> Int
converteHoras (H p1 p2) = p1*60+p2

--d) converter um valor em minutos para horas;
converteMinutosHoras :: Int -> Hora
converteMinutosHoras l = H (div l 60) (mod l 60) --mod é o resto da divisão

--e) calcular a diferenca entre duas horas (cujo resultado deve ser o numero de minutos)
diferencaHoras :: Hora -> Hora -> Int
diferencaHoras (H p1 p2) (H p3 p4) = (p1-p3)*60 + (p2-p4)

--f) adicionar um determinado numero de minutos a uma dada hora
adicionarMinutos :: Hora -> Int -> Hora
adicionarMinutos (H h m) min = converteMinutosHoras (converteHoras (H h m) + min)

---------------------------------------
---------       Ficha 3       ---------
---------------------------------------

-- Gestao de informacao em listas

--a) Testar se uma etapa est ́a bem constru ́ıda (i.e., o tempo de chegada  ́e superior ao de partida e as horas s ̃ao v ́alidas).
etapaBemConstruida :: Etapa -> Bool
etapaBemConstruida (p1,p2) = horaValida p1 && horaValida p2 && horaTempoDeChegadaSuperior p1 p2 

--b) Testa se uma viagem est ́a bem constru ́ıda (i.e., se para cada etapa, o tempo de chegada  ́e superior ao de partida, e se a etapa seguinte come ̧ca depois da etapa anterior ter terminado)
viagemBemConstruida :: Viagem -> Bool
viagemBemConstruida [] = True
viagemBemConstruida [x] = etapaBemConstruida x
viagemBemConstruida ((x,y):(x1,y1):t) = etapaBemConstruida (x,y) && horaTempoDeChegadaSuperior y x1 && viagemBemConstruida ((x1,y1):t) 

--c) Calcular a hora de partida e de chegada de uma dada viagem. 
partidaEChegada :: Viagem -> (Hora,Hora)
partidaEChegada n = (fst (head n), snd (last n)) --fst pega o primeiro elemento do par, snd pega o ultimo elemento do par * Manipulação de Tuplos

{-
partidaEChegada :: Viagem -> (Hora,Hora)
partidaEChegada n = (x,y)
    where (x,_) = head n
    where (_,y) = last n
-}

tempoVi :: Viagem -> Hora
tempoVi [] = H 0 0
tempoVi ((x,y):t) = converteMinutosHoras (diferencaHoras y x + converteHoras (tempoVi t))

{-
--d) Dada uma viagem v ́alida, calcular o tempo total de viagem efectiva
tempoViagemEfetiva :: Viagem -> Hora
tempoViagemEfetiva [] = H 0 0
tempoViagemEfetiva ((p1, p2):t) = adicionaHoras (converteMinutosHoras (diferencaHoras p2 p1)) (tempoViagemEfetiva t)
-}
tempoViagemEfetiva :: Viagem -> Hora
tempoViagemEfetiva [] = H 0 0
tempoViagemEfetiva v = converteMinutosHoras totalMinutos
    where
        (H h1 m1) = fst (head v)
        (H h2 m2) = snd (last v)
        totalMinutos = (h2*60 + m2) - (h1*60 + m1)


adicionaHoras :: Hora -> Hora -> Hora
adicionaHoras (H p1 p2) (H p3 p4) = H (p1 + p2 + (div (p2+p4) 60)) (mod (p2+p4) 60)

--e) Calcular o tempo total de espera.
tempoEspera :: Viagem -> Hora
tempoEspera [] = H 0 0
tempoEspera [_] = H 0 0
tempoEspera ((x,y):(x1,y1):t) = converteMinutosHoras (diferencaHoras x1 y + converteHoras (tempoEspera ((x1,y1):t)))

--f) Calcula o tempo total da viagem
tempoTotalViagem' :: Viagem -> Hora
tempoTotalViagem' v = converteMinutosHoras (diferencaHoras (snd (last v)) (fst (head v))) --O ultimo elemento do ultimo par - o primeiro elemento do primeiro par

{-
tempoTotalViagem :: Viagem -> Hora
tempoTotalViagem v = adicionaHoras (tempoViagemEfetiva v) (tempoEspera v)

tempoTotalViagem' :: Viagem -> Hora
tempoTotalViagem' v = diferencaHoras hf hi
    where (hi,hf) = partidaEChegada v
-}

--2)
type Poligonal = [Ponto]

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

--Auxiliares
--a) calcula a distˆancia de um ponto ao eixo vertical.
posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = r * cos a

--b) calcula a distˆancia de um ponto ao eixo horizontal.
posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar r a) = r * sin a

--c) calcula a distˆancia de um ponto `a origem.
raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x^2 + y^2)
raio (Polar r a) = r

--d) calcula o ˆangulo entre o vector que liga a origem a um ponto e o eixo horizontal.
angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (y/x)
angulo (Polar r a) = a

--e) calcula a distˆancia entre dois pontos
dist1 :: Ponto -> Ponto -> Double
dist1 p1 p2 = sqrt ((x1 - x)^2 + (y1-y)^2)
    where x = posx p1
          y = posy p1
          x1 = posx p2
          y1 = posy p2

--a) Defina a fun ̧c ̃ao para calcular o comprimento de uma linha poligona
calculaComprimento :: Poligonal -> Double
calculaComprimento [] = 0
calculaComprimento [_] = 0
calculaComprimento (x1:x2:t) = (dist1 x1 x2) + calculaComprimento t 

--b) Defina uma fun ̧c ̃ao para testar se uma dada linha poligonal  ́e ou n ̃ao fechada.
testaLinha :: Poligonal -> Bool
testaLinha p = length p >= 3 && head p == last p --Como tem deriving eq o haskell compara ponto como Cartesiano 3 4 == Cartesiano 3 4 que é True

--c) Defina a função triangula :: Poligonal -> [Figura] que, dada uma linha poligonal fechada e convexa, calcule uma lista de triângulos cuja soma das áreas seja igual à àrea delimitada pela linha poligonal. O tipo Figura é idêntico ao definido na Ficha 1. triangula :: Poligonal -> [Figura]
data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

triangula :: Poligonal -> [Figura]
triangula (p1:p2:p3:ps)
    | p1 == p3 = [] --Parar a recursão quando chegamos ao ultimo traingulo, em um poligonal fechado se p1 é igual a p3 não á mais triangulos a formar
    | otherwise = Triangulo p1 p2 p3 : triangula (p1:p3:ps) -- Cria o primeiro traingulo, e chama recursivamente avançando para os proximos
triangula _ = []

--d) Defina uma fun ̧c ̃ao para calcular a  ́area delimitada por uma linha poligonal fechada e convexa.

--Auxiliares
area :: Figura -> Double
area (Circulo _ r) = pi*r^2
area (Rectangulo p1 p2) = abs (posx p2 - posx p1) * abs (posy p2 - posy p1)
area (Triangulo p1 p2 p3) = let a = dist1 p1 p2
                                b = dist1 p2 p3
                                c = dist1 p3 p1
                                s = (a+b+c) / 2 -- semi-perimetro
                                in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron

areaPol :: Poligonal -> Double
areaPol p = areaTris (triangula p)

areaTris :: [Figura] -> Double
areaTris [] = 0
areaTris (h:t) = area h + areaTris t

--e) Defina a fun ̧c ̃ao triangula :: Poligonal -> [Figura] que, dada uma linha poligonal fechada e convexa, calcule uma lista de triˆangulos cuja soma das  ́areas seja igual `a  ́area delimitada pela linha poligonal. O tipo Figura  ́e idˆentico ao definido na Ficha 1
mover :: Poligonal -> Ponto -> Poligonal
mover pol p = p : pol

--f) Defina a função zoom :: Double -> Poligonal -> Poligonal que, dada um fator de escala e uma linha poligonal, dê como resultado uma linha poligonal semelhante e com o mesmo ponto inicial mas em que o comprimento de cada segmento de reta é multiplicado pelo fator dado.
zoom :: Double -> Poligonal -> Poligonal
zoom z (h:t) = mover (doZoom z h t) h

doZoom :: Double -> Ponto -> Poligonal -> Poligonal
doZoom z p [] = []
doZoom z p (h:t) = Cartesiano ((x - xp) * z + xp) ((y - yp) * z + yp) : doZoom z p t
    where x = posx h
          y = posy h
          xp = posx p
          yp = posy p

--4)
data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

--a) Defina a fun ̧cao que, dado um nome, um email e uma agenda, acrescenta essa informacao á agenda.
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email [] = [(nome, [Email email])]
acrescEmail nome email ((n,cs):t)
    | nome == n = (n, Email email : cs) : t
    | otherwise = (n, cs) : acrescEmail nome email t


--b) Defina a fun ̧c ̃ao verEmails :: Nome -> Agenda -> Maybe [String] que, dado um nome e uma agenda, retorna a lista dos emails associados a esse nome. Se esse nome n ̃ao existir na agenda a fun ̧c ̃ao deve retornar Nothing.
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails name [] = Nothing
verEmails name ((n,cs):t) | name == n = Just (soEmails cs)
                          | otherwise = verEmails name t

soEmails :: [Contacto] -> [String]
soEmails [] = []
soEmails (Email s : t) = s : soEmails t
soEmails (_:t) = soEmails t

--c)Defina a fun ̧c ̃ao consTelefs :: [Contacto] -> [Integer] que, dada uma lista de contactos, retorna a lista de todos os n ́umeros de telefone dessa lista (tanto telefones fixos como telemoveis
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (Casa n : t) = n : consTelefs t
consTelefs (Trab n : t) = n : consTelefs t
consTelefs (Tlm n : t) = n : consTelefs t

--d) Defina a fun ̧c ̃ao casa :: Nome -> Agenda -> Maybe Integer que, dado um nome e uma agenda, retorna o n ́umero de telefone de casa (caso exista)
casa :: Nome -> Agenda -> Maybe Integer
casa name [] = Nothing
casa name ((n,cs):t) | name == n = soCasa cs
                     | otherwise = casa name t
soCasa :: [Contacto] -> Maybe Integer
soCasa [] = Nothing
soCasa (Casa n : t) = Just n
soCasa (_:t) = soCasa t

--4)Pretende-se guardar informa ̧c ̃ao sobre os anivers ́arios das pessoas numa tabela que
-- associa o nome de cada pessoa `a sua data de nascimento. Para isso, declarou-se a
-- seguinte estrutura de dados

type Dia = Int
type Mes = Int
type Ano = Int
data Data = D Dia Mes Ano deriving Show
type TabDN = [(Nome,Data)]

--a) Defina a fun ̧c ̃ao procura :: Nome -> TabDN -> Maybe Data, que indica a data de nascimento de uma dada pessoa, caso o seu nome exista na tabela.
procura :: Nome -> TabDN -> Maybe Data
procura name [] = Nothing
procura name ((x,y):t) | name == x = Just y
                       | otherwise = procura name t 

--b) Defina a fun ̧c ̃ao idade :: Data -> Nome -> TabDN -> Maybe Int, que calcula a idade de uma pessoa numa dada data
idade :: Data -> Nome -> TabDN -> Maybe Int
idade _ _ [] = Nothing
idade date name ((x,y):t) | name == x = Just (calculaIdade date y) 
                          | otherwise = idade date name t
            
calculaIdade :: Data -> Data -> Int
calculaIdade (D d m a) (D d1 m1 a1) | m < m1 || m == m1 && d1 < d = a - a1 --Já fez anos ou seja date é depois do aniversario
                                    | otherwise = a - a1 - 1--Ainda não fez anos

--c)Defina a fun ̧c ̃ao anterior :: Data -> Data -> Bool, que testa se uma data  ́e anterior a outra data.
anterior :: Data -> Data -> Bool
anterior (D d m a) (D d1 m1 a1) = a < a1 || a == a1 && m < m1 || a == a1 && m1 == m && d < d1
{-
Exemplo
ghci> anterior (D 10 6 2024) (D 15 6 2024)
True
-}
--d) Defina a fun ̧c ̃ao ordena :: TabDN -> TabDN, que ordena uma tabela de datas de nascimento, por ordem crescente das datas de nascimento.
ordena :: TabDN -> TabDN 
ordena [] = []
ordena (h:t) = ordena (ordenaAux h t)

ordenaAux :: (Nome, Data) -> TabDN -> TabDN
ordenaAux (n,d) [] = [(n,d)]
ordenaAux (n,d) ((n1,d1):t) | anterior d d1 = (n,d) : (n1, d1) : t
                            | otherwise = (n1,d1) : ordenaAux (n,d) t

--e) Defina a função porIdade:: Data -> TabDN -> [(Nome,Int)] que apresenta o nome e a idade das pessoas, numa dada data, por ordem crescente da idade das pessoas.
porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade date tabela = porIdadeAux date (ordena tabela)

porIdadeAux :: Data -> TabDN -> [(Nome,Int)]
porIdadeAux _ [] = []
porIdadeAux d ((n,dn):t) = porIdadeAux d t ++ [(n, calculaIdade d dn)]

--5)
data Movimento = Credito Float | Debito Float deriving Show
data Extracto = Ext Float [(Data, String, Movimento)] deriving Show

--Construa a função extValor :: Extracto -> Float -> [Movimento] que produz uma lista de todos os movimentos (créditos ou débitos) superiores a um determinado valor.
extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ []) _ = []  -- caso base: lista vazia de movimentos
extValor (Ext n ((_,_,p):t)) a | getValor p > a = p : extValor (Ext n t) a
                               | otherwise = extValor (Ext n t) a

getValor :: Movimento -> Float
getValor (Credito x) = x
getValor (Debito x) = x

--b Defina a fun ̧c ̃ao filtro :: Extracto -> [String] -> [(Data,Movimento)] que retorna informa ̧c ̃ao relativa apenas aos movimentos cuja descri ̧c ̃ao esteja inclu ́ıda na lista fornecida no segundo parˆametro.
filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext _ []) _ = []
filtro (Ext n ((a,d,p):t)) desc | elem d desc = (a,p) : filtro (Ext n t) desc
                                | otherwise = filtro (Ext n t) desc

--c) Defina a fun ̧c ̃ao creDeb :: Extracto -> (Float,Float), que retorna o total de cr ́editos e de d ́ebitos de um extracto no primeiro e segundo elementos de um par, respectivamente.
creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ []) = (0,0)
creDeb (Ext n ((_,_,Credito x):t)) = (x + xr, tr)
        where (xr,tr) = creDeb(Ext n t)
creDeb (Ext n ((_,_,Debito x):t)) = (x, x + tr)
        where (xr,tr) = creDeb(Ext n t)

--ou

creDeb' :: Extracto -> (Float,Float)
creDeb' (Ext _ []) = (0,0)
creDeb' (Ext si ((_,_,mov):t)) = (c + cr, d + dr)
    where (cr,dr) = creDeb (Ext si t)
          (c,d) = case mov of Credito x -> (x,0)
                              Debito x -> (0,x)

--d) Defina a fun ̧c ̃ao saldo :: Extracto -> Float que devolve o saldo final que resulta da execu ̧c ̃ao de todos os movimentos no extracto sobre o saldo inicial
saldo :: Extracto -> Float
saldo (Ext n []) = n
saldo (Ext n ((_,_,Credito x):t)) = saldo (Ext (n+x) t) --Credito adiciona
saldo (Ext n ((_,_,Debito x):t)) = saldo (Ext (n-x) t) --Debito tira
