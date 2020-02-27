neglist [] = 0
neglist (x:xs) | x < 0 = 1 + neglist xs
            | otherwise = neglist xs

main = do
    inputdata <- getContents
    print $ neglist $ map (read :: String->Int) (lines inputdata)