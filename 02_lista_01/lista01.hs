import Data.Char -- toUpper, toLower
import Data.List -- sort

-- Questao 1
menorDeDois :: (Ord a) => a -> a -> a
menorDeDois x y
    | x < y = x
    | otherwise = y
-- menorDeDois x y = min x y

-- Questao 2
menorDeTres :: (Ord a) => a -> a -> a -> a
menorDeTres x y z
    | x < y && x < z = x
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
pertence n (x:xs)
    | x == n = True
    | otherwise = pertence n xs
-- pertence u x = elem x u

-- Questao 7
total :: [a] -> Int
total [] = 0
total (_:xs) = 1 + total xs

-- Questao 8
maior :: (Ord a) => [a] -> a
maior (x:xs)
    | null xs || maior xs < x = x
    | otherwise = maior xs

-- Questao 9
frequencia :: (Eq a) => a -> [a] -> Int
frequencia n [] = 0
frequencia n (x:xs)
    | x == n = 1 + frequencia n xs
    |otherwise = frequencia n xs

-- Questao 10
unico :: (Eq a) => a -> [a] -> Bool
unico _ [] = False
unico n u = frequencia n u == 1

-- Questao 11
maioresQue :: Int -> [Int] -> [Int]
maioresQue n [] = []
maioresQue n (x:xs)
    |  n < x = x:maioresQue n xs
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
corpo (x:[_]) = [x]
corpo (x:xs) = x:corpo xs
-- corpo (x:xs)
-- | (length xs) == 1 = [x]
-- | otherwise = [x] ++ corpo xs
-- corpo u = init u
-- corpo u = take ((length u) - 1) u

-- Questao 15
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique [x] = [x]
unique all@(x:xs)
    | unico x all = x:unique xs
    | otherwise = unique xs
-- unique u = nub u

-- Questao 16
menores :: Int -> [Int] -> [Int]
menores 0 _ = []
menores n u = [x | x <- u, x <= el]
    where el = if n < length u then sort u !! (n-1) else maxBound :: Int

-- Questao 17
alter :: Int -> [Int]
alter n = [x*y | x <- [1..n], y <- [1,-1]]

-- Questao 18
reverso :: [a] -> [a]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]
-- reverso u = reverse u

-- Questao 19
divide :: [a] -> Int -> ([a],[a])
divide u n = (take n u, drop n u)

-- Questao 20
intercal :: [a] -> [a] -> [a]
intercal [] b = b
intercal a [] = a
intercal (x:xs) (y:ys) = [x, y] ++ intercal xs ys

-- Questao 21
uniao' :: (Eq a) => [a] -> [a] -> [a]
uniao' u [] = []
uniao' u (x:xs)
    | frequencia x u == 0 = x:uniao' u xs
    | otherwise = uniao' u xs

uniao :: (Eq a) => [a] -> [a] -> [a]
uniao u [] = u
uniao [] x = x
uniao u x = u ++ uniao' u x
-- uniao u x = union u x

-- Questao 22
intersec :: (Eq a) => [a] -> [a] -> [a]
intersec _ [] = []
intersec [] _ = []
intersec (x:xs) b
    | frequencia x b /= 0 = x:intersec xs b
    | otherwise = intersec xs b

-- Questao 23
sequencia :: Int -> Int -> [Int]
sequencia 0 _ = []
sequencia n m = [m + x | x <- [0..(n-1)]]

-- Questao 24
inserir :: Int -> [Int] -> [Int]
inserir n [] = [n]
inserir n (x:xs)
    | x > n = [n, x] ++ xs
    | otherwise = x:inserir n xs

-- Questao 25
isSorted :: [Int] -> Bool
isSorted [] = True
isSorted (x:y:xs)
    | x < y && not (null xs) = True && isSorted (y:xs)
    | x < y = True
    | otherwise = False

-- Questao 26
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y < x] ++ [x] ++ qsort [y | y <- xs, y >= x]

-- Questao 27
rotEsq :: Int -> [a] -> [a]
rotEsq 0 u = u
rotEsq n (x:xs)
    | n == 1 = xs ++ [x]
    | otherwise = rotEsq (n-1) $ xs ++ [x]

-- Questao 28
rotDir :: Int -> [a] -> [a]
rotDir 0 u = u
rotDir n u = reverse $ rotEsq n $ reverse u

-- Questao 29
upper :: String -> String
upper s = [toUpper x | x <- s]

-- Questao 30
titulo :: String -> String
titulo s = unwords $ [toUpper (head x):[toLower y | y <- tail x] | x <- words s]

-- Questao 31
selec :: [a] -> [Int] -> [a]
selec _ [] = []
selec u (x:xs) = (u !! x):selec u xs

-- Questao 32
isPalind :: String -> Bool
isPalind [] = False
isPalind (_:[]) = True
isPalind s
    | head s == last s = True && isPalind (init $ tail s)
    | otherwise = False

-- Questao 33
primo :: Int -> Bool
primo n = length [x | x <- [1..n], mod n x == 0] == 2

-- Questao 34
sdig :: Int -> Int
sdig 0 = 0
sdig n = mod n 10 + sdig (div n 10)

-- Questao 36
cont _ [] = 0
cont x (y:ys)
    | x == y = 1 + cont x ys
    | otherwise = 0 

compac :: [Int] -> [[Int]]
compac [] = []
compac (x:xs)
    | tes == 0 = [x]:compac xs
    | otherwise = [tes+1, x]:compac (drop tes xs)
    where tes = cont x xs

-- Questao 37
splitints :: [Int] -> ([Int], [Int])
splitints u = ([x | x <- u, odd x], [x | x <- u, even x])
