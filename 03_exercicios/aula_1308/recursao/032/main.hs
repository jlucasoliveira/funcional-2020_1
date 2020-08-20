sequencia :: Int -> Int -> [Int]
sequencia 1 m = [m]
sequencia n m = sequencia (n-1) m ++ [m+n-1]

main :: IO ()
main = do
    a <- readLn :: IO Int
    b <- readLn :: IO Int
    print $ sequencia a b