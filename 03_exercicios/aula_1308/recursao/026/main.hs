alter :: Int -> [Int]
alter 0 = []
alter n = alter (n-1) ++ [n,(-n)]
