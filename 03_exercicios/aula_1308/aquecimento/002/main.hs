iguais :: Int -> Int -> Int -> Int
iguais a b c
    | a == b && a == c = 3
    | a == b || b == c || a == c = 2
    | otherwise = 0