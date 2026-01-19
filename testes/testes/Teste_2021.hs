{-
1. Apresente uma defini ̧c ̃ao recursiva da fun ̧c ̃ao (\\) :: Eq a => [a] -> [a] -> [a] que retorna a lista
resultante de remover (as primeiras ocorrˆencias) dos elementos da segunda lista da primeira. Por exemplo,
(\\) [1,2,3,4,5,1,2] [2,3,4,1,2] == [5,1].
-}
delete :: Eq a => a -> [a] -> [a]
delete n [] = []
delete n (h:t) | n == h = t
               | otherwise = h : delete t

doubleBars :: Eq a => [a] -> [a] -> [a]
doubleBars p [] = p
doubleBars [] p = []
doubleBars l (h:t) = doubleBars (delete h l) t

calcula :: MSet a -> ([a],Int)
calcula [] = ([],0)
calcula ((x,n):t) = if aux (x,n) t then  else (x:xs, n + total)
  where
    (xs,total) = calcula t

aux :: (a,Int) -> MSet a -> Bool
aux (a,i) [] = ([a],i)
aux (a,i) ((x,y):t) = a == x 