intersec :: (Eq a) => [a] -> [a] -> [a]
intersec a b = [x | x <- a, elem x b]

uniao :: (Eq a) => [a] -> [a] -> [a]
uniao a b = a ++ [x | x <- b, notElem x a]

inserir :: (Ord a) => a -> [a] -> [a]
inserir n [] = [n]
inserir n y@(x:xs) = if x > n then n:y else x:inserir n xs
-- inserir n u = [x | x <- u, x <= n] ++ [n] ++ [x | x <- u, x > n]
