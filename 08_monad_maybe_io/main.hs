printMultipleLines :: Int -> IO ()
printMultipleLines n
   | n <= 0 = return ()
   | otherwise = do
       putStrLn "Ola mundo"
       printMultipleLines (n-1)

contaRep :: String -> String -> Int
contaRep [] _ = 0
contaRep (x:xs) m = if elem x m then (z+1) else z
    where z = contaRep xs m

codigoUltron :: IO ()
codigoUltron = do
    l1 <- getLine
    l2 <- getLine
    let pal = words l2
    let p = map (`contaRep` l1) pal
    mapM_ (\s -> do {
        let tm = length (fst s) in
        if tm == snd s then
            putStr "chefe "
        else if fromIntegral tm / 2.0 < fromIntegral(snd s) then
            putStr "ultron "
        else 
            putStr "pessoa "
    }) (zip pal p)

