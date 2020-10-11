import Polinomio (Polinomio, polZero, isPolZero, grau, coefLider, restoPol, consPol)

makePol :: (Eq a, Num a) => [a] -> Polinomio a
makePol xs = foldr f polZero (zip xs [n, (n-1)..]) 
    where
        f (x,y) a = if x == 0 then a else consPol x y a
        n = length xs - 1



derivada :: (Num a, Eq a) => Polinomio a -> Polinomio a
derivada p
    | isPolZero p = polZero
    | grau p - 1 < 0 = (derivada $ restoPol p)
    | otherwise = consPol (fromIntegral(grau p) * (coefLider p)) (grau p - 1) (derivada $ restoPol p)
