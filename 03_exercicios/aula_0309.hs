buscaPos :: Int -> [Int] -> Int
buscaPos x xs = if null res then -1 else head res
    where
        u = zip xs [0..]
        res = [snd z | z <- u, fst z == x]

type AssocList k v = [(k, v)]

getValue :: Int -> AssocList Int String -> Maybe String
getValue k mapa = if null u then Nothing else Just $ head u
    where u = [y | (x, y) <- mapa, x == k]

data Calc = Calc {battery :: Int} deriving (Show)

divide :: Calc -> Int -> Int -> Either String Int
divide c x y 
    | battery c == 0 = Left "Bateria zerada"
    | y == 0         = Left "Divisao por zero"
    | otherwise      = Right $ div x y