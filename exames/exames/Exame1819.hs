--1)
--a)
isSorted :: Ord a => [a] -> Bool
isSorted []       = True
isSorted [_]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

--b)
inits1 :: [a] -> [[a]]
inits1 [] = [[]]
inits1 l = (inits1 (init l)) ++ [l]

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' (h:t) = [] : map (h:) (inits' t)

--2)
maximumMB :: (Ord a) => [Maybe a]-> Maybe a 
maximumMB [] = Nothing
maximumMB (h:t) = maxAux h (maximumMB t)

maxAux :: (Ord a) => Maybe a -> Maybe a -> Maybe a
maxAux Nothing y = y
maxAux x Nothing = x
maxAux (Just x) (Just y) | x >= y = Just x
                         | otherwise = Just y

maximumMB' :: Ord a => [Maybe a] -> Maybe a 
maximumMB' [] = Nothing
maximumMB' (Nothing:t) = maximumMB' t
maximumMB' (Just x:t) =
    case maximumMB' t of
        Nothing -> Just x
        Just y  -> Just (max x y)

maximumMB1 :: (Ord a) => [Maybe a] -> Maybe a
maximumMB1 l = Just $ maximum (aux l) 
    where aux [] = []
          aux ((Just x):t) = x : aux t
          aux (Nothing:t)  =     aux t 

--3)
data LTree a = Tip a | Fork (LTree a) (LTree a)
--a)
listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork e d) = listaLT e ++ listaLT d

--b)
instance Show a => Show (LTree a) where
    show t = folhaLinha t 0

folhaLinha :: Show a => LTree a -> Int -> String
folhaLinha (Tip a) n = replicate n '.' ++ show a ++ "\n"
folhaLinha (Fork e d) n = folhaLinha e (n+1) ++ folhaLinha d (n+1)