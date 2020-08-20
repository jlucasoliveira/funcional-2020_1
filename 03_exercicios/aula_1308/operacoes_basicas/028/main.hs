divide :: [a] -> Int -> ([a], [a])
divide u n = (left, right)
    where
        left  = take n u
        right = drop n u