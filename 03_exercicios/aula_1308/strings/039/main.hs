upper :: [Char] -> [Char]
upper [] = []
upper (x:xs) = if elem x ['a'..'z'] then (['A'..'Z'] !! idx):upper xs else x:upper xs
    where idx = length (takeWhile (x/=) ['a'..'z'])
-- upper s = [toUpper(x) | x <- s ]
