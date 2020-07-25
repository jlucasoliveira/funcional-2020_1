import Data.List

-- Q1
menorDeDois :: (Ord a) => a -> a -> a
menorDeDois x y = if x > y then y else x
-- menorDeDois = min
-- menorDeDois x y
--      | x > y = y
--      | otherwise = x

-- Q2
menorDeTres :: (Ord a) => a -> a -> a -> a
menorDeTres x y z
    | x < y && x < z = x
    | y < x && y < z = y
    | otherwise = z

-- menorDeTres = menorDeDois . menorDeDois

-- Q3
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial (n-1)
-- fatorial n = product [1..n]

-- Q4
fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- Q5
elemento :: (Ord a) => Int -> [a] -> a
elemento n u = u !! n
-- elemento' u 0 = head u
-- elemento' u n = elemento' (tail u) (n-1)

--Q6
pertence :: (Eq a) => a -> [a] -> Bool
pertence x []     = False
pertence x (y:ys) = if y == x then True else pertence x ys
-- pertence u x = elem u x

-- Q7
total :: [a] -> Integer
total [] = 0
total u  = 1 + (total $ tail u)

-- Q8
maior :: (Ord a) => [a] -> a
maior (x:[]) = x
maior (x:xs) = if maior xs < x then x else maior xs

-- Q9
frequencia :: (Eq a) => a -> [a] -> Int
frequencia x []     = 0
frequencia x (y:ys) = if y == x then 1 + resto else resto
    where resto = frequencia x ys

-- Q10
unico :: (Eq a) => a -> [a] -> Bool
unico x u = if length ys /= 1 then False else True
    where ys = [z | z <- u, z == x]
    -- where ys = filter (==x) u

-- Q11
maioresQue :: (Ord a) => a -> [a] -> [a]
maioresQue _ []     = []
maioresQue x (y:ys) = if x < y then y:maioresQue x ys else maioresQue x ys
    -- maioresQue x u = filter (>x) u
    -- maioresQue x u = [z | z <- u, z > x]

-- Q12
concat :: [a] -> [a] -> [a]
concat a b = a ++ b

-- Q13
calda :: [a] -> [a]
calda (_:xs) = xs

-- Q14
corpo :: [a] -> [a]
corpo (x:[y]) = [x]
corpo (x:xs)  = x:corpo xs 

-- Q15
unique' :: (Eq a) => [a] -> [a] -> [a]
unique' [] aux = aux
unique' (x:xs) aux = if not (elem x aux) then unique' xs (aux++[x]) else unique' xs aux

unique :: (Eq a) => [a] -> [a]
unique u = unique' u []

-- Q16
menores :: (Ord a) => Int -> [a] -> [a]
menores n u = [z | z <- u, elem z final]
    where final = take n $ sort u

-- Q17
alter :: Int -> [Int]
alter n = [x*y | x <- [1..n], y <- [1, -1]]

-- Q18
reverso :: [a] -> [a]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

-- Q19
divide :: [a] -> Int -> ([a], [a])
divide u n = (a, b)
    where
        a = take n u
        b = drop n u

-- Q20
intercal :: [a] -> [a] -> [a]
intercal x []          = x
intercal [] y          = y 
intercal (x:xs) (y:ys) = [x,y] ++ intercal xs ys

-- Q21
uniao :: (Eq a) => [a] -> [a] -> [a]
uniao a b = a ++ [z | z <- b, not $ elem z a]

-- Q22
intersec :: (Eq a) => [a] -> [a] -> [a]
intersec _ []     = []
intersec [] _     = []
intersec (x:xs) b = if elem x b then x:intersec xs b else intersec xs b

-- Q23
sequencia :: Int -> Int -> [Int]
sequencia n m = [m..limit]
    where limit = m + (n-1)

-- Q24
inserir :: Int -> [Int] -> [Int]
inserir x [] = [x]
inserir x all@(y:ys) = if y > x then x:all else y:inserir x ys

-- Q25
isSorted :: (Ord a) => [a] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:ys) = if x <= y then True && (isSorted $ y:ys) else False

-- Q26
qsort :: (Ord a) => [a] -> [a]
qsort []      = []
qsort [x]     = [x]
qsort (x:xs)  = qsort [y | y <- xs, y < x] ++ [x] ++ qsort [z | z <- xs, z >= x]

-- Q27
rotEsq :: Int -> [a] -> [a]
rotEsq n u = drop per u ++ take per u
    where per = mod n 5

-- Q28
rotDir :: Int -> [a] -> [a]
rotDir n u = take per u ++ drop per u
    where per = mod n 5
