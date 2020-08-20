fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n-1)
-- fatorial n = product [0..n]