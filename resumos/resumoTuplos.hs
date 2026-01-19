{-
Tuplos em Haskell
O que são?

Estruturas que agrupam valores possivelmente de tipos diferentes.
Escrevem-se entre parênteses, separados por vírgulas.
Diferente das listas, o tamanho e os tipos são fixos.
-}
--Exemplos:
(1, 2)            -- par de inteiros
("rui", 20)       -- String e Int
(True, 'a', 3.14) -- triplo com Bool, Char e Float

{-Diferenças em relação a listas
Listas: homogêneas (só um tipo), tamanho variável.
Tuplos: heterogêneos (tipos diferentes), tamanho fixo.
-}
[1,2,3]     -- lista de Int
(1, "abc")  -- tuplo (Int, String)

{-Acesso a tuplos
Funções pré-definidas para pares:
-}
fst (1,2)   == 1
snd (1,2)   == 2


--Para tuplos maiores, normalmente usa-se pattern matching:
--Exemplos:
nomeIdade :: (String, Int) -> String
nomeIdade (nome, idade) = nome ++ " tem " ++ show idade ++ " anos"

exemplo = nomeIdade ("Ana", 30)  -- Printa "Ana tem 30 anos"

{-Pattern matching

Permite desconstruir tuplos em variáveis:
-}
coord :: (Int, Int) -> Int
coord (x, y) = x + y -- coord (2,3) == 5

{-Operações úteis

Trocar a ordem (pode ser feito manualmente):
-}
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

--Trabalhar com listas de tuplos:
nomes = [("Ana",20), ("Rui",30), ("Maria",25)]

-- extrair só os nomes:
map fst nomes   == ["Ana","Rui","Maria"]
-- extrair só as idades:
map snd nomes   == [20,30,25]
-- filtrar pessoas com mais de 25 anos:
filter (\(_, idade) -> idade > 25) nomes
== [("Rui",30)]
{-
Tuplos em funções

Uma função pode receber vários parâmetros de duas formas:
Normal: f x y
Via tuplo: g (x,y)
-}
--Exemplo:
soma :: (Int,Int) -> Int
soma (a,b) = a+b -- soma (3,4) == 7

{-Resumo final:
Tuplos agrupam valores fixos (podem ser de tipos diferentes).
Usados muito em pares chave-valor, coordenadas, dados compostos.
Manipulação: fst, snd, pattern matching, map, filter em listas de tuplos.
-}