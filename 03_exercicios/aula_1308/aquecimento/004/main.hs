countNeg :: [Int] -> Int
countNeg [] = 0
countNeg (x:xs) = if x < 0 then 1 + countNeg xs else countNeg xs
-- countNeg x = length [y | y <- x, y < 0]