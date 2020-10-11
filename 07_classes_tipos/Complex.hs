import Numeric (showFFloat)

data Complex = Complex {real :: Float, img :: Float}

instance Show Complex where
    show (Complex a b) = (showFFloat (Just 3) a "") ++ " + " ++ (showFFloat (Just 3) b "") ++ "i"

-- https://en.wikipedia.org/wiki/Sign_function
divComplex :: Complex -> Complex -> Complex
divComplex (Complex a b) (Complex c d) = (Complex ((a*c + b*d)/(c^2 + d^2)) ((b*c - a*d)/(c^2 + d^2)))

instance Num Complex where
    (+) c1 c2       = (Complex ((real c1) + (real c2)) ((img c1) + (img c2)))
    (-) c1 c2       = (Complex ((real c1) - (real c2)) ((img c1) - (img c2)))
    (*) (Complex x y) (Complex u v) = (Complex (x*u - y*v) (x*v + y*u)) -- https://www2.clarku.edu/faculty/djoyce/complex/mult.html
    negate (Complex x y) = (Complex (negate x) (negate y))
    abs (Complex x y)= (Complex (sqrt (x^2 + y^2)) 0)
    signum n = divComplex n (abs n)
    fromInteger n   = (Complex (fromInteger n) 0)

