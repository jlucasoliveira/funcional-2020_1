interior :: [a] -> [a]
interior u = drop 1 $ take (length u - 1) u