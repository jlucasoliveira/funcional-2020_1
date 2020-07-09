-- Questao 06
pertence :: (Eq a) => a -> [a] -> Bool
pertence _ [] = False
pertence x (y:ys)
    | x == y = True
    | otherwise = pertence x ys


-- Questao 09
frequencia :: (Eq a) => a -> [a] -> Int
frequencia _ [] = 0
frequencia x (y:ys)
    | x == y = 1 + frequencia x ys
    | otherwise = frequencia x ys


-- Questao 15
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
    | frequencia x xs > 0 = unique xs
    | otherwise = x:unique xs


-- Questao 20
intercal :: (Eq a) => [a] -> [a] -> [a]
intercal [] x = x
intercal y [] = y
intercal (x:xs) (y:ys) = [x, y] ++ intercal xs ys