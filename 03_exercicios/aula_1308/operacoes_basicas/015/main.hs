pertence :: (Eq a) => a -> [a] -> Bool
pertence _ [] = False
pertence x (y:ys)
    | x == y = True
    | otherwise = pertence x ys
-- pertence x xs = null $ filter (==x) xs
-- pertence x xs = null [y | y <- xs, y == x]
-- pertence x xs = elem x xs