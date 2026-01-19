{-
1. O que são listas?

Estruturas homogêneas → todos os elementos do mesmo tipo.
Escrevem-se entre []


-} 
--Exemplo:
[1,2,3,4]
"abc"   -- lista de chars

{-
Construtores principais: 
* [] → lista vazia
* : → operador "cons" (coloca elemento na frente)
-} 
--Exemplo:
1 : [2,3,4]  ==  [1,2,3,4]

{-
2. Acesso e padrões
Cabeça (head): primeiro elemento
Cauda (tail): lista sem a cabeça
-} 
--Exemplos:
head [1,2,3]  == 1

tail [1,2,3]  == [2,3]

--Padrões:

[]       -- lista vazia
(h:t)    -- separa cabeça e cauda

-- Operações principais
--Concatenar
[1,2] ++ [3,4] == [1,2,3,4]

--Adicionar na frente
5 : [1,2,3] == [5,1,2,3]

--Acesso por índice
[10,20,30] !! 1 == 20   -- começa no zero

--4. Funções comuns de listas
--Comprimento: length
 [1,2,3] == 3

--Vazia:
null [] == True

--Inverter:
reverse [1,2,3] == [3,2,1]

--Tomar / descartar:
take 3 [1..10] == [1,2,3]
drop 3 [1..10] == [4,5,6,7,8,9,10]

--Maior/menor: 
maximum [4,1,9] == 9

--Somatório/produto:
sum [1,2,3] == 6
product [1,2,3,4] == 24

--5. Gestão com predicados
--Filtrar:
filter even [1..10] == [2,4,6,8,10]

--Transformar:
map (*2) [1,2,3] == [2,4,6]

--xistência / todos:
any odd [2,4,6,7]    == True
all even [2,4,6,8]   == True

--6. Geração de listas
--Intervalos:
[1..5]    == [1,2,3,4,5]
[2,4..10] == [2,4,6,8,10]

--Infinitas (lazy evaluation):
[1..]          -- todos os naturais
take 5 [1..]   == [1,2,3,4,5]
repeat 7       -- [7,7,7,7,...]
cycle [1,2]    -- [1,2,1,2,1,2,...]
replicate 4 9  == [9,9,9,9]


{- 7. List compreension
Resumo:
É uma forma compacta e expressiva de construir listas a partir de uma lista base (ou várias) uma regra de transformação (o que você quer fazer com cada elemento),
condições (filtros) para escolher quais elementos entram no resultado.

Extrutura geral: [ expressão | gerador , filtros ]
-}

--Exemplos:
[x | x <- [1,2,3,4,5]]
-- resultado: [1,2,3,4,5]

[x^2 | x <- [1..5]]
-- resultado: [1,4,9,16,25]

--8. Resumo 
--Construção:
[], :, ++
--Acesso:
head, tail, !!
--Transformação:
map, filter, reverse, take, drop
--Redução:
sum, product, maximum, minimum, length
--Predicados:
any, all, null
--Geração:
intervalos [a..b], repeat, replicate, cycle
--Compreensão:
[f x | x <- lista, condição]