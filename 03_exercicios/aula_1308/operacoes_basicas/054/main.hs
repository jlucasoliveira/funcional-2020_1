swap :: [a] -> Int -> Int -> [a]
swap u ini end = left ++ (verIdx end u) ++ middle ++ (verIdx ini u) ++ right
    where
        left = take ini u
        right = drop (end + 1) u
        middle = drop (ini+1) $ take end u
        verIdx x u = if x >= 0 && x < length u then [u !! x] else []  