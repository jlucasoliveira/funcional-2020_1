gangorra :: Int -> Int -> Int -> Int -> Int
gangorra p1 c1 p2 c2
    | l1 == l2  = 0
    | l1 > l2   = -1
    | otherwise = 1
    where
        l1 = p1 * c1
        l2 = p2 * c2