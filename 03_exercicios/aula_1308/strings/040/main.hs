toUpper :: Char -> Char
toUpper c = if elem c ['a'..'z'] then (['A'..'Z'] !! idx) else c
        where idx = length (takeWhile (c/=) ['a'..'z'])

toLower :: Char -> Char
toLower c = if elem c ['A'..'Z'] then (['a'..'z'] !! idx) else c
        where idx = length (takeWhile (c/=) ['A'..'Z'])

words' :: String -> [String]
words' [] = []
words' s = if null atual then words' resto else [atual] ++ words' resto
    where
        atual = takeWhile (/=' ') s
        resto = drop (length atual + 1) s

unwords' :: [String] -> String
unwords' [x] = x
unwords' (x:xs) = x ++ " " ++ unwords' xs

titulo :: [Char] -> [Char]
titulo xs = unwords' [cap x | x <- words' xs]
    where
        cap (y:ys) = toUpper(y):[toLower(z) | z <- ys]
