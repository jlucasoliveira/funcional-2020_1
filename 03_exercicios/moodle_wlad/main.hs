concatenaFold :: [[Int]] -> [Int]
concatenaFold u = foldr (++) [] u

inverteFold :: [a] -> [a]
inverteFold xs = foldr (\x z -> z++[x]) [] xs


paridadeFold :: [Bool] -> Bool
paridadeFold xs = mod (foldr (\x b -> if x then b + 1 else b ) 0 xs) 2 == 0


isVowel :: Char -> Bool
isVowel c = elem c ['a','e','i','o','u', 'A', 'E', 'I', 'O', 'U']

duplicarFold :: String -> String
duplicarFold s = foldr (\x y -> if isVowel x then [x,x]++y else x:y) "" s

filtraAplicaFold :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaFold f p xs = foldr (\x z -> if p x then (f x):z else z) [] xs

mapFold :: (a->b) -> [a] -> [b]
mapFold f xs = foldr (\x z -> (f x):z) [] xs

removeLista :: (Eq a) => [a] -> [a] -> [a]
removeLista xs ys = foldr (\x z -> if elem x xs then z else x:z) [] ys

acertosFold :: String -> String -> Int
acertosFold xs ys = foldr (\x z -> if f x then z + 1 else z) 0 [0..tam]
    where
        f i = (xs !! i) == (ys !! i)
        tam = length xs -1

descompactaFold ::  [(a, b)] -> ([a], [b])
descompactaFold z = foldr (\(a,b) (xs,ys) -> (a:xs, b:ys)) ([], []) z
