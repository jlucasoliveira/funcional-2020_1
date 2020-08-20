splitints :: [Int] -> ([Int], [Int])
splitints xs = (filter odd xs, filter even xs)
-- splitints xs = ([x | x <- xs, odd x], [x | x <- xs, even x])