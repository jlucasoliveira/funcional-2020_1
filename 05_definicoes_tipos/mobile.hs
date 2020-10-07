data Mobile = Pendente Int | Barra Mobile Mobile deriving (Eq, Show)

balanceado :: Mobile -> Bool
balanceado (Pendente _) = True
balanceado (Barra a b) = (balanceado a) && (balanceado b) && (peso a == peso b)
    where
        peso (Pendente a) = a
        peso (Barra a b) = peso a + peso b

-- makeMobile

makeMobile :: [Int] -> Mobile
makeMobile [x] = Pendente x
makeMobile xs = (Barra (makeMobile left) (makeMobile right))
    where
        metade = length xs `div` 2
        left = take metade xs
        right = drop metade xs
