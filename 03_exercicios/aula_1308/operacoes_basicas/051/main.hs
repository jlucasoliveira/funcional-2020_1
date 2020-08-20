paridade :: [Bool] -> Bool
paridade u = mod (length $ filter (\x -> x) u) 2 /= 0