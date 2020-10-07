splits :: [a] -> [([a],[a])]
splits xs = [count x xs | x <- zip [0..length xs] xs]

count :: (Int, a) -> [a] -> ([a], [a])
count n xs = (takeWhile (\x ->  ) xs, dropWhile () xs)