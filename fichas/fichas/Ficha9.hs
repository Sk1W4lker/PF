module Ex1 where 

import System.Random 
import Data.List

type FilePath = String 

--a)
bingo :: IO ()
bingo = do bingoAux [1..10]
           putStrLn "BINGO !!!"

bingoAux :: [Int] -> IO ()
bingoAux [] = return ()
bingoAux list = do 
    numSort <- randomRIO (1,90 :: Int)
    if numSort `elem` list 
    then do putStr "Prima ENTER para sortear um número: "
            getChar 
            print numSort 
            bingoAux (delete numSort list)
    else bingoAux list
                    
--b) 
mastermind :: IO ()
mastermind = do 
    n1 <- randomRIO (0,9 :: Int) 
    n2 <- randomRIO (0,9 :: Int) 
    n3 <- randomRIO (0,9 :: Int) 
    n4 <- randomRIO (0,9 :: Int) 
    let seq = [n1,n2,n3,n4] 
--  print seq
    mastermindAux seq 

mastermindAux :: [Int] -> IO ()
mastermindAux seq = do 
    putStr "Introduza 4 números de 1 a 9, espaçados: "
    nums <- getLine 
    let separados = words nums
        numeros = map (\ x -> read x :: Int) separados
        intersecao = numeros `intersect` seq 
        (certos,errados) = valoresCertos seq intersecao numeros 
    putStrLn ("Valores certos na posição certa: " ++ show certos) 
    putStrLn ("Valores certos na posição errada: " ++ show errados)
    if certos == 4 
    then putStrLn "Acertou em todos os números, PARABÉNS!!!" 
    else mastermindAux seq 

-- Função que devolve o número de valores na posicao certa e o numero de valores na posicao errada
valoresCertos :: [Int] -> [Int] -> [Int] -> (Int,Int)
valoresCertos seq intersecao introduzido = 
    somaPares $ map (\ n -> if null (elemIndices n seq `intersect` elemIndices n introduzido) -- se os indices de n forem iguais em ambas as listas, então o valor está nas posições corretas, indices porque n pode ser repetido 
                            then (0,1)
                            else (1,0) ) intersecao 

-- Função que soma a primeira componente e segunda componente de uma lista de pares. Ex: [(1,2),(4,5)] = (1+4,2+5)
somaPares :: [(Int,Int)] -> (Int,Int)
somaPares list = (comp1,comp2)
    where comp1 = sum $ map fst list 
          comp2 = sum $ map snd list 

type FilePath = String 

data Aposta = Ap [Int] (Int,Int) 

aposta :: Aposta 
aposta = Ap [34,10,43,49,17] (3,4)

aposta' :: Aposta 
aposta' = Ap [12,34,1,4,40] (7,3)

-- Função que torna uma Aposta numa string
showAp :: Aposta -> String 
showAp (Ap numeros (x,y)) = 
    "    Aposta\n" ++ "Números: " ++ 
    concat (map (\ n -> (show n) ++ " ") numeros) ++ 
    "\n" ++ "Estrelas: " ++ show x ++ " " ++ show y

-- Instância de Show para o data type Aposta
instance Show Aposta where 
    show = showAp 

--a)
valida :: Aposta -> Bool 
valida (Ap list (x,y)) = length ([1..50] `intersect` list) == 5 &&
                         x `elem` [1..9] && y `elem` [1..9] && x/=y

--b) 
comuns :: Aposta -> Aposta -> (Int,Int)
comuns (Ap lis1 (e1,e2)) (Ap lis2 (e3,e4)) = (numerosComum,estrelasComum)
    where numerosComum = length $ lis1 `intersect` lis2 
          estrelasComum = length $ [e1,e2] `intersect` [e3,e4]

--c) 
--i) 
eqAposta :: Aposta -> Aposta -> Bool 
eqAposta ap1 ap2 = comuns ap1 ap2 == (5,2) 

instance Eq Aposta where 
    (==) = eqAposta 

--ii) 
premio :: Aposta -> Aposta -> Maybe Int 
premio ap1 ap2 = case certos of 
                      (5,2) -> Just 1 
                      (5,1) -> Just 2
                      (5,0) -> Just 3 
                      (4,2) -> Just 4 
                      (4,1) -> Just 5 
                      (4,0) -> Just 6 
                      (3,2) -> Just 7 
                      (2,2) -> Just 8 
                      (3,1) -> Just 9 
                      (3,0) -> Just 10 
                      (1,2) -> Just 11 
                      (2,1) -> Just 12 
                      (2,0) -> Just 13 
                      (_,_) -> Nothing
    where certos = comuns ap1 ap2 

--d)                                                
--i) 
leAposta :: IO Aposta 
leAposta = do putStr "Insira 5 números de 1 a 50, separados por espaço: " 
              list <- getLine 
              let separados = words list
                  numeros = map (\ x -> read x :: Int) separados 
              putStr "Insira uma estrela de 1 a 9: "
              e1 <- getChar 
              putStr "\nInsira uma estrela de 1 a 9: "
              e2 <- getChar 
              putStrLn ""
              let estrela1 = read [e1] :: Int
                  estrela2 = read [e2] :: Int 
                  aposta = Ap numeros (estrela1,estrela2)
              if valida aposta 
              then do print aposta 
                      return aposta
              else do putStrLn "Aposta inválida, tente novamente: " 
                      leAposta

--ii) 
joga :: Aposta -> IO ()
joga chave = do aposta <- leAposta 
                let prize = premio aposta chave 
                putStrLn ("O seu prémio é: " ++ case prize of 
                                                     Nothing -> "Não tem prémio." 
                                                     Just x -> show x )
--              print chave

--e) 
geraChave :: IO Aposta 
geraChave = do 
    e1 <- randomRIO (1,9) 
    e2 <- randomRIO (1,9)
    if e1 == e2 
    then geraChave 
    else do nums <- geraChaveAux []
            return (Ap nums (e1,e2))

-- Função que gera uma lista, de 5 números, de 1 a 50, válida 
geraChaveAux :: [Int] -> IO [Int]
geraChaveAux nums = do 
    if length nums == 5 
    then return nums 
    else do n <- randomRIO (1,50)
            if n `elem` nums 
            then geraChaveAux nums 
            else geraChaveAux (n:nums) 
                            
--f) 
main :: IO ()
main = do ch <- geraChave
          ciclo ch

ciclo :: Aposta -> IO ()
ciclo chave = do 
    selecao <- menu 
    if selecao == "1"
    then do joga chave 
            main
    else if selecao == "2" 
         then main 
         else putStrLn "Bom jogo!!!"            

menu :: IO String
menu = do putStrLn menutxt
          putStr "Opcao: "
          getLine
    where menutxt = unlines ["",
                             "Apostar ........... 1",
                             "Gerar nova chave .. 2",
                             "",
                             "Sair .............. 0"]