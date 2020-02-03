{-Implementation of the Exclusive Or (XOR) function-}

xor :: (Integral a) => a -> a -> a
xor x y
    | x ==0 && y == 1 = 1
    | x ==1 && y == 0 = 1
    |otherwise = 0

xorList :: (Integral a) => [a] -> [a] -> [a]
xorList xs ys = [xor x y | (x,y) <- zip xs ys]

parity :: (Integral a) => [a] -> a
parity [] = 0
parity (x:xs) = foldl xor x xs

binary x = bin x
    where
        bin 0 = []
        bin y = let (q,r) = (quot y 2, mod y 2)
            in bin q ++ [r]
