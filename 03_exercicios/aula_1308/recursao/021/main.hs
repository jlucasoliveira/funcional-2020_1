concatena :: [a] -> [a] -> [a]
concatena a [] = a
concatena [] b = b
concatena (x:xs) b = x:concatena xs b
-- concatena a b = a ++ b
