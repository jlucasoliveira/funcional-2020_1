total :: [a] -> Int
total []      = 0
total (x:xs)  = 1 + total xs
-- total u = sum $ map (\x -> div x x) u