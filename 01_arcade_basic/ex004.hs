somaImpares lista = sum [x | x <- lista, (not (mod x 2 == 0))]

main = do
    inputdata <- getContents
    print $ somaImpares $ map (read :: String -> Int) (lines inputdata)