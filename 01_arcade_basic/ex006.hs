size [] = 0
size (x: xs) = 1 + size xs 

final n xs = drop ((size xs) - n) xs

main = do
    inputdata <- getContents
    let entrada = lines inputdata
    let n       = read( entrada !! 0 )
    print $ final n $ map (read :: String->Int) (tail entrada)