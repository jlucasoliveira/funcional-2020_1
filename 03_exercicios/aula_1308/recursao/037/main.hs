rotEsq :: Int -> String -> String
rotEsq _ [] = []
rotEsq 0 s  = s
rotEsq n (x:xs) = (rotEsq (n-1) xs) ++ [x]

main :: IO ()
main = do
    a <- readLn :: IO Int
    b <- getLine
    print $ rotEsq a b