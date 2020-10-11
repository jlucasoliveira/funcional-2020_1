data Mes = Janeiro | Fevereiro | Marco | Abril | Maio | Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro
        deriving (Show, Ord, Eq, Enum) 

data Date = MkDate {dia :: Int, mes :: Mes, ano :: Int}

instance Show Date where
    show d = show (dia d) ++ " de " ++ show (mes d) ++ " de " ++ show (ano d)

instance Eq Date where
    (==) d1 d2 = dia d1 == dia d2 && mes d1 == mes d2 && ano d1 == ano d2

instance Ord Date where
    (<=) d1 d2 = (ano d1 < ano d2) || (mes d1 < mes d2 && ano d1 <= ano d2)
                || (dia d1 <= dia d2 && mes d1 <= mes d2 && ano d1 <= ano d2)
                