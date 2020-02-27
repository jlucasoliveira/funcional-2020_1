interior lista = tail (init lista) 

main = do
    inputdata <- getContents
    print $ interior $ map (read :: String->Int) (lines inputdata)