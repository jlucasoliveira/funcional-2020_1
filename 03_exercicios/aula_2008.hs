data Point = Point {x :: Float, y :: Float } deriving (Show)
data Shape = Circle {p :: Point, r :: Float} | Retangle {p1 :: Point, p2 :: Point} deriving (Show)

-- https://math.stackexchange.com/questions/198764/how-to-know-if-a-point-is-inside-a-circle


inside :: Point -> Shape -> Bool
inside p (Circle c r) = distancia c p <= r
    where
        distancia :: Point -> Point -> Float
        distancia p1 p2 = sqrt ((x p1 - x p2)**2 + ((y p1 - y p2))**2)
inside p (Retangle pos dim) = inX && inY
    where
        inX = x p <= (maximum $ map x [pos, dim]) && x p >= (minimum $ map x [pos, dim])
        inY = y p <= (maximum $ map y [pos, dim]) && y p >= (minimum $ map y [pos, dim])