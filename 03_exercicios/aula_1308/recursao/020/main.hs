maioresQue :: (Ord a) => a -> [a] -> [a]
maioresQue x xs = filter (>x) xs
