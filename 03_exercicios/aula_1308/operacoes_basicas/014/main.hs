elemento :: (Ord a) => Int -> [a] -> a
elemento n xs = if n >= 0 then xs !! n else xs !! revIdx
    where revIdx = length xs + n