sublist :: Int -> Int -> [a] -> [a]
sublist ini end u = drop (idx ini u) $ take (idx end u) u 
    where idx x u = if x >= 0 then x else length u + x