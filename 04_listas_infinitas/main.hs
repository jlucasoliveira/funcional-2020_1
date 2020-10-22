
kolakoski' :: [Int] -> [Int]
kolakoski' at@(a:b:c) = 
    where prox = 1 + mod a 2

kolakoski :: [Int]
kolakoski = kolakoski' [1,2,2]

mescla2 :: (Ord a) => [a] -> [a] -> [a]
mescla2 x [] = x
mescla2 [] y = y
mescla2 xa@(x:xs) ya@(y:ys)
    | x == y = x:mescla2 xs ys
    | x < y = x:mescla2 xs ya
    | otherwise = y:mescla2 xa ys

mescla3 :: (Ord a) => [a] -> [a] -> [a] -> [a]
mescla3 a b c = mescla2 (mescla2 a b) c

hamming' :: [Int] -> [Int]
hamming' ant = hamming' $ mescla2 ant mult
    where mult = mescla3 (map (*2) ant) (map (*3) ant) (map (*5) ant)

hamming :: [Int]
hamming = hamming' [1]

--- FUNCIONAM
collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n
    | mod n 2 == 0 = n:collatz(div n 2)
    | otherwise = n:collatz(3*n+1)

fechoKleene' :: [[a]] -> [a] -> [[a]]
fechoKleene' ant alf = let temp = [x++[y] | x <- ant, y <- alf] in temp ++ (fechoKleene' temp alf)

fechoKleene :: [a] -> [[a]]
fechoKleene a = []:fechoKleene' [[]] a

ehPrimo :: Integer -> Bool
ehPrimo 0 = True
ehPrimo 2 = True
ehPrimo n = length [x | x <- 2:[3,5..lim], mod n x == 0] == 0
    where lim = if n > 20 then round $ sqrt $ fromInteger n else n-1

primos :: [Integer]
primos = 2:[x | x <- [3,5..], ehPrimo x]

ehPalindromo :: String -> Bool
ehPalindromo [] = True
ehPalindromo (_:[]) = True
ehPalindromo u
    | head u == last u = ehPalindromo $ (init.tail) u
    | otherwise = False

goldbach :: Integer -> [(Integer,Integer,Integer)]
goldbach n = [(x, y x, x- (y x)) | x <- [4,6..(n-1)]]
    where
        y x = head $ filter (\p -> ehPrimo (x-p)) (takeWhile (<n) primos)

primosPalindromo :: [Integer]
primosPalindromo = [x | x <- 2:[3,5..], ehPrimo x && (ehPalindromo $ show x)]

primosGemeos :: [(Integer,Integer)]
primosGemeos = [(x, x+2) | x <- [3,5..], ehPrimo x && ehPrimo (x+2)]