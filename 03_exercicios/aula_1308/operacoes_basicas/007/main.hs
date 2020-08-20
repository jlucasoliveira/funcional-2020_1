somaImpares :: [Int] -> Int
somaImpares xs = sum $ filter odd xs
-- somaImpares xs = sum $ [x | x <- xs, odd x]