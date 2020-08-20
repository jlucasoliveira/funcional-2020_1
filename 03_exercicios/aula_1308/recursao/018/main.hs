frequencia :: (Eq a) =>  a -> [a] -> Int
frequencia _ [] = 0
frequencia x (y:ys)
    | x == y    = 1 + frequencia x ys
    | otherwise = frequencia x ys
-- frequencia x u = length $ filter (==x) u
-- frequencia x u = length [y | y <- u, y == x]
