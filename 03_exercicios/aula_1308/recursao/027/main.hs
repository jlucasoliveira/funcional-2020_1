reverso :: [Int] -> [Int]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

main :: IO ()
main = do
    a <- readLn :: IO [Int]
    print $ reverso a
