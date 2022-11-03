lmax :: Ord a => [a] -> a
lmax [x] = x
lmax (x:x':xs) = lmax ((if x >= x' then x else x'):xs)

lmin :: Ord a => [a] -> a
lmin [x] = x
lmin (x:x':xs) = lmin ((if x <= x' then x else x'):xs)

primechecker :: Int -> Bool
primechecker1 = False
primechecker 2 = True
primechecker n | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False | otherwise = True


main = print (lmin [2, 6, 3, 70, 1])