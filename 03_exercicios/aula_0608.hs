comparar x y = compare x y

compareCom100 = (`comparar` 100)

applyNvezes :: Int -> (a -> a) -> a -> a
applyNvezes 0 _ x = x
applyNvezes 1 f x = f x
applyNvezes n f x = f (applyNvezes (n-1) f x)

res n l = n ++ " " ++ (show l)
comb no le = zipWith res no le