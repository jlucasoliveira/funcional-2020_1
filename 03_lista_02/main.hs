-- 01
paridade :: [Bool] -> Bool
paridade u = length(filter (\x -> x) u) `mod` 2 /= 0

-- 02
rev :: Int -> Int
rev n
    | n < 10    = n
    | otherwise = (mod n 10 * (10^contarCasas (res n))) + rev (res n) 
    where
        contarCasas n = if n < 10 then 1 else 1 + contarCasas (res n)
        res x = (div x 10)

-- 03
deletee :: (Eq a) => a -> [a] -> [a]
deletee _ [] = []
deletee x (y:ys) = if x == y then ys else y:deletee x ys

-- 04
swap :: (Ord a) => [a] -> Int -> Int -> [a]
swap u p q = left ++ (ctt u q) ++ middle ++ (ctt u p) ++ right
    where
        left = take p u
        right = drop (q+1) u
        middle = drop (p+1) (take q u)
        ctt x idx = if idx >= 0 && idx < length x then [x !! idx] else []

-- 05
fstchar :: (Ord a) => [a] -> Int
fstchar [] = 0
fstchar (x:y:ys)
    | x < y = 0
    | y > x = 1 + fstchar (y:ys)

--scdchar :: (Ord a) => [a] -> Int

nextPerm :: (Ord a) => [a] -> [a]
nextPerm [] = []
nextPerm (x:y:ys)
    | x < y = nextPerm ys

-- 07
buscaBin' :: (Ord a) => [a] -> a -> Int -> Int -> Int
buscaBin' u x i e
    | i > e         = -1
    | x == (u!!idx) = idx
    | x > (u!!idx)  = buscaBin' u x (idx+1) e
    | otherwise     = buscaBin' u x i (idx-1)
    where idx       = div (e+i) 2

buscaBin :: (Ord a) => [a] -> a -> Int
buscaBin u x = buscaBin' u x 0 (length u - 1)

-- 08
-- Verifica se um número é par excluíndo da verificação os pares (para melhor performace)
isPrime :: Int -> Bool
isPrime 0 = True
isPrime 1 = False
isPrime 2 = True
isPrime n = mod n 2 /= 0 && length [x | x <- [3,5..(n-1)], mod n x == 0] == 0

-- Retorna o próximo número primo
nextPrime :: Int -> Int
nextPrime x = if isPrime (x+1) then (x+1) else nextPrime (x+1)

-- Calcula até onde p é fator de x
divs :: Int -> Int -> Int
divs 0 _ = 0
divs x p = if mod x p == 0 then 1 + divs (div x p) p else 0

factors' :: Int -> Int -> [(Int, Int)]
factors' 1 _ = []
factors' x p = if checked == 0 then factors' x next else [(p, checked)] ++ (factors' rest next)
    where
        next    = nextPrime p
        checked = divs x p
        rest    = div x (p^checked)

factors :: Int -> [(Int, Int)]
factors n = factors' n 2

-- 09
listacc :: [Int] -> [Int]
listacc [] = []
listacc u = listacc (init u) ++ [sum u]
