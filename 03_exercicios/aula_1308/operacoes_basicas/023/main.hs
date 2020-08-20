corpo :: [a] -> [a]
corpo [x] = []
corpo (x:xs) = x:corpo xs
-- corpo u = init u