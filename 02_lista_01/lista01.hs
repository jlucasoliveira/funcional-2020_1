-- Questao 1
menorDeDois :: (Ord a) => a -> a -> a
menorDeDois x y | x < y = x
    | otherwise = y
-- menorDeDois x y = min x y

-- Questao 2
menorDeTres :: (Ord a) => a -> a -> a -> a
menorDeTres x y z | x < y && x < z = x
    | y < x && y < z = y
    | otherwise = z
-- menorDeTres x y z = min x $ min y z

-- Questao 3
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial (n-1)
-- fatorial n = product [1..n]

-- Questao 4
fibonacci :: Int -> Int
fibonacci 1 = 0
fibonacci 2 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- Questao 5
elemento :: Int -> [a] -> a
elemento n u = u !! n

-- Questao 6
pertence :: (Eq a) => a -> [a] -> Bool
pertence n [] = False
pertence n (x:xs) | x == n = True
    | otherwise = pertence n xs
-- pertence u x = elem x u

-- Questao 7
total :: [a] -> Int
total [] = 0
total (x:xs) = 1 + total xs

-- Questao 8
maior :: (Ord a) => [a] -> a
maior (x:xs)| (length xs == 0 || (maior xs) < x) = x
    | otherwise = maior xs

-- Questao 9
frequencia :: (Eq a) => a -> [a] -> Int
frequencia n [] = 0
frequencia n (x:xs) | x == n = 1 + (frequencia n xs)
    |otherwise = 0 + (frequencia n xs)

-- Questao 10
unico :: (Eq a) => a -> [a] -> Bool
unico n [] = False
unico n u = frequencia n u == 1 

-- Questao 11
maioresQue :: Int -> [Int] -> [Int]
maioresQue n [] = []
maioresQue n (x:xs) |  n < x = [x] ++ maioresQue n xs
    | otherwise = maioresQue n xs

-- Questao 12
concat' :: [a] -> [a] -> [a]
concat' u v = u ++ v
-- concat' u v = concat u v

-- Questao 13
calda :: [a] -> [a]
calda (_:xs) = xs
-- calda u = tail u

-- Questao 14
corpo :: [a] -> [a]
corpo [] = []
corpo (x:xs) | (length xs) == 1 = [x]
    | otherwise = [x] ++ corpo xs
-- corpo u = init u
-- corpo u = take ((length u) - 1) u

-- Questao 17
alter :: Int -> [Int]
alter n = [x*y | x <- [1..n], y <- [1,-1]]

-- Questao 18
reverso :: [a] -> [a]
reverso [] = []
reverso (x:xs) = (reverso xs) ++ [x]
-- reverso u = reverse u

-- Questao 19
divide :: [a] -> Int -> ([a],[a])
divide u n = (take n u, drop n u)

-- Questao 20
intercal :: [a] -> [a] -> [a]
intercal [] b = b
intercal a [] = a
intercal (x:xs) (y:ys) = [x, y] ++ intercal xs ys

-- Questao 22
intersec :: (Eq a) => [a] -> [a] -> [a]
intersec _ [] = []
intersec [] _ = []
intersec (x:xs) b | frequencia x b /= 0 = x:(intersec xs b)
    | otherwise = intersec xs b

-- Questao 23
sequencia :: Int -> Int -> [Int]
sequencia 0 _ = []
sequencia n m = [m + x | x <- [0..(n-1)]]

-- Questao 24
inserir :: Int -> [Int] -> [Int]
inserir n [] = [n]
inserir n (x:xs) | x > n = [n, x] ++ xs
    | otherwise = x:inserir n xs

-- Questao 25
isSorted :: [Int] -> Bool
isSorted [] = True
isSorted (x:y:xs) | x < y && length xs > 0 = True && isSorted (y:xs)
    | x < y = True
    | otherwise = False

-- Questao 27
rotEsq :: Int -> [a] -> [a]
rotEsq 0 u = u
rotEsq n (x:xs) | n == 1 = xs ++ [x]
    | otherwise = rotEsq (n-1) $ xs ++ [x]
