unico :: (Eq a) => a -> [a] -> Bool
unico _ [] = False
unico x (y:ys)
    | x == y    = True && not (unico x ys)
    | otherwise = unico x ys
-- unico x u = length (filter (==x) u) == 1
-- unico x u length [y | y <- u, x == y] == 1
