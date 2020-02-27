max3 x y z | (x > y && x > z) = x
    | (z > x && z > y) = z
    | otherwise = y

main = do
    line1 <- getLine 
    line2 <- getLine
    line3 <- getLine

    let x = read line1 :: Int
    let y = read line2 :: Int
    let z = read line3 :: Int

    print $ max3 x y z