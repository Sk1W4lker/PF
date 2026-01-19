--1)
a :: [a] -> Int -> a
a (h:_) 0 = h
a (_:t) n = a t (n-1)

--2)
data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x,y) (Norte:t) = (x+1,y) t
posicao (x,y) (Sul:t) = (x-1,y) t
posicao (x,y) (Este:t) = (x,y+1) t
posicao (x,y) (Oeste:t) = (x,y-1) t
    