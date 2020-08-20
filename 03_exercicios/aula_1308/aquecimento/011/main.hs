min3 :: Int -> Int -> Int -> Int
min3 x y z
    | x > y && y < z = y
    | x < y && x < z = x
    | otherwise      = z