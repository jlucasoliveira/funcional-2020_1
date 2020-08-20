intersec :: (Eq a) => [a] -> [a] -> [a]
intersec _ [] = []
intersec [] _ = []
intersec a b = [x | x <- a, elem x b]