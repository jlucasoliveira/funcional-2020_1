unique' :: (Eq a) => [a] -> [a] -> [a]
unique' [] aux     = aux
unique' (x:xs) aux = if notElem x aux then unique' xs (aux++[x]) else unique' xs aux

unique :: (Eq a) => [a] -> [a]
unique u = unique' u []
