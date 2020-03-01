-- Questao 1
menorDeDois x y | x < y = x
    | otherwise = y
-- menorDeDois x y = min x y

-- Questao 2
menorDeTres x y z | x < y && x < z = x
    | y < x && y < z = y
    | otherwise = z
-- menorDeTres x y z = min x $ min y z

-- Questao 3
fatorial 0 = 1
fatorial n = n * fatorial (n-1)
-- fatorial n = product [1..n]

-- Questao 4
fibonacci 1 = 0
fibonacci 2 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- Questao 5
elemento n u = u !! n

-- Questao 6
pertence n [] = False
pertence n (x:xs) | x == n = True
    | otherwise = pertence n xs
-- pertence u x = elem x u

-- Questao 7
total [] = 0
total (x:xs) = 1 + total xs

-- Questao 8
maior (x:xs)| (length xs == 0 || (maior xs) < x) = x
    | otherwise = maior xs

-- Questao 9
frequencia n [] = 0
frequencia n (x:xs) | x == n = 1 + (frequencia n xs)
    |otherwise = 0 + (frequencia n xs)

-- Questao 10 NOT-WORKING
unico n [] = False
unico n (x:xs) | not (unico n xs) && n == x = True
    | otherwise = False

-- Questao 11
maioresQue n [] = []
maioresQue n (x:xs) |  n < x = [x] ++ maioresQue n xs
    | otherwise = maioresQue n xs

-- Questao 12
concat' u v = u ++ v
-- concat' u v = concat u v

-- Questao 13
calda (_:xs) = xs
-- calda u = tail u

-- Questao 14
corpo [] = []
corpo (x:xs) | (length xs) == 1 = [x]
    | otherwise = [x] ++ corpo xs
-- corpo u = init u
-- corpo u = take ((length u) - 1) u

-- Questao 15 NOT-WORKING
unique [] = []
unique (x:xs) | frequencia x xs == 0 = [x] ++ unique (xs ++ [x])
    | otherwise = unique xs

-- Questao 16 NOT-WORKING


-- Questao 17
alter n = [x*y | x <- [1..n], y <- [1,-1]]

-- Questao 18
reverso [] = []
reverso (x:xs) = (reverso xs) ++ [x]
-- reverso u = reverse u

-- Questao 19
-- divide u 0 = ([], u)
-- divide (x:xs) n | n == 1 = ()
divide u n = (take n u, drop n u)

-- Questao 20
intercal [] b = b
intercal a [] = a
intercal (x:xs) (y:ys) = [x, y] ++ intercal xs ys

-- Questao 21 NOT-WORKING
uniao a (x:xs) | frequencia x a == 0 = a ++ [x] ++ uniao a xs
    | otherwise = a ++ uniao a xs

-- Questao 22
intersec _ [] = []
intersec [] _ = []
intersec (x:xs) b | frequencia x b /= 0 = x:(intersec xs b)
    | otherwise = intersec xs b

-- Questao 23
sequencia 0 _ = []
sequencia n m = [m + x | x <- [0..(n-1)]]

-- Questao 24
inserir n [] = [n]
inserir n (x:xs) | x > n = [n, x] ++ xs
    | otherwise = x:inserir n xs

-- Questao 25
isSorted [] = True
isSorted (x:y:xs) | x < y && length xs > 0 = True && isSorted (y:xs)
    | x < y = True
    | otherwise = False

