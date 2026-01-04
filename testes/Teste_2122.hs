zip1 :: [a] -> [b] -> [(a,b)]
zip1 [] _ = []
zip1 _ [] = []
zip1 (h:t) (h1:t1) = (h,h1) : zip1 t t1

preCrescente1 :: Ord a => [a] -> [a]
preCrescente1 [] = []
preCrescente1 [x] = [x]
preCrescente1 (h:x:t) | h < x = h : preCrescente1 (x:t)
                      | otherwise = [h]

amplitude :: [Int] -> Int
amplitude [] = 0
amplitude l = (maximum l) - (minimum l)

amplitude' :: [Int] -> Int
amplitude' x = amplitudeAux x 0

amplitudeAux :: [Int] -> Int -> Int
amplitudeAux x n = 