type Row = [Float]
data Matrix = Matrix {ncols :: Int, nrows :: Int, rows :: [Row]}

-- matriz de zeros
zeroMatrix :: Int -> Int -> Matrix
zeroMatrix l c = Matrix c l (take l (repeat (take c (repeat 0.0))))

-- matriz de uns
oneMatrix :: Int -> Int -> Matrix
oneMatrix l c = Matrix c l (take l (repeat (take c (repeat 1.0))))

-- matriz identidade : recebe ordem
identMatrix :: Int -> Matrix
identMatrix n = Matrix n n ([[if y == x then 1 else 0 | y <- [1..n]] | x <- [1..n]])

somaRow :: Row -> Row -> Row
somaRow [] [] = []
somaRow (x:xs) (y:ys) = (x+y):somaRow ys xs

-- soma duas matrizes
sumMatrix :: Matrix -> Matrix -> Matrix
sumMatrix x y
    | valida = (Matrix (ncols x) (nrows x) [somaRow x y | (x,y) <- (zip (rows x) (rows y))]) 
    | otherwise = error "As matrizes devem ter as mesmas dimensoes!"
    where
        valida = ncols x == ncols y && nrows x == nrows y

-- produto de escalar por matriz
prodScalar :: Float -> Matrix -> Matrix
prodScalar f m = (Matrix (ncols m) (nrows m) [[f*x | x <- xs] | xs <- rows m])

transposta :: [Row] -> [Row]
transposta xs = if (null.head) xs then [] else [head x | x <- xs]:(transposta [tail x | x <- xs])

-- produto entre matrizes
prodMatrix :: Matrix -> Matrix -> Matrix
prodMatrix (Matrix cx rx x) (Matrix cy ry y)
    | ry == cx = (Matrix rx cy [[ sum (zipWith (*) a b) | b <- transposta y] | a <- x])
    | otherwise = error "A #colunas da 1a matriz tem que ser igual a #linhas da 2a!"

-- transforma listas de listas de
-- floats numa matriz
listToMatrix :: [Row] -> Matrix
listToMatrix [] = (Matrix 0 0 [])
listToMatrix a = (Matrix (length a) (length $ head a) a)

instance Show Matrix where
    show m = unlines ["| " ++ (unwords [(show z)++" "| z <- x]) ++ "|" | x <- rows m]