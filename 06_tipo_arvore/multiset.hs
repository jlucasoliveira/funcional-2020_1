data MultiSet a = MultiSet [(a,Int)] deriving (Show, Eq, Ord)
data MConj a = Vazia | No a Int (MConj a) (MConj a) deriving (Show, Eq)

conta :: Eq a => a -> [a] -> (Int, [a])
conta x xs = (length [y | y <- xs, y == x], [y | y <- xs, y /= x])

compac :: Ord a => [a] -> a -> [(a, Int)]
compac [] _ = []
compac xs m = if qtd == 0 then compac unsel unmin else (m, qtd):compac unsel unmin
    where
        res = conta m xs
        qtd = fst res
        unsel = snd res
        unmin = (minimum unsel)

makeMultiSet :: Ord a => [a] -> MultiSet a 
makeMultiSet xs = MultiSet (compac xs (minimum xs))

insere' :: Ord a => a -> [(a,Int)] -> [(a,Int)]
insere' a [] = [(a, 1)]
insere' a ys@((x,y):xs)
    | x == a = (x, y+1):xs
    | x > a = (a, 1):ys
    | otherwise = (x,y):(insere' a xs)

insere :: Ord a => a -> MultiSet a -> MultiSet a 
insere a (MultiSet xs) = (MultiSet (insere' a xs))

delete' :: Ord a => a -> Int -> [(a,Int)] -> [(a,Int)]
delete' _ _ [] = []
delete' a n (xy@(x,y):ys)
    | x == a = if y <= n then [] else [(a, y-n)]
    | otherwise = xy: delete' a n ys

delete :: Ord a => a -> Int -> MultiSet a -> MultiSet a
delete a n (MultiSet m) = MultiSet (delete' a n m)

sucessor :: Ord a => MConj a -> (a, Int)
sucessor (No a b Vazia _) = (a, b)
sucessor (No _ _ _ dir) = sucessor dir

remove' :: Eq a => MConj a -> MConj a
remove' no@(No _ _ esq dir)
    | esq == Vazia && dir == Vazia = Vazia
    | esq == Vazia = dir
    | dir == Vazia = esq
    | otherwise = no

remove :: Ord a => a -> Int -> MConj a -> MConj a
remove _ _ Vazia = Vazia
remove x q no@(No a b esq dir)
    | a == x = if b <= q then temp else (No a (b-q) esq dir)
    | a < x = (No a b esq (remove x q dir))
    | a > x = (No a b (remove x q esq) dir)
    where
        suc = sucessor dir
        an = fst suc
        bn = snd suc
        temp = let test = remove' no in if test == no then (No an bn esq (remove an bn dir)) else test