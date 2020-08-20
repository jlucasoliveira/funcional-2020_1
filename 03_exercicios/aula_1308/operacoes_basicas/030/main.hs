uniao :: (Eq a) => [a] -> [a] -> [a]
uniao a [] = a
uniao [] b = b
uniao a b = a ++ [x | x <- b, notElem x a]