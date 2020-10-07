data ArvoreExpressao = No (Int->Int->Int) ArvoreExpressao ArvoreExpressao | Folha Int
data Expr = Val Int | Soma Expr Expr | Mult Expr Expr | Div Expr Expr | Sub Expr Expr | Mod Expr Expr deriving (Read,Eq,Show)

fromExpr :: Expr -> ArvoreExpressao
fromExpr (Val e) = Folha e
fromExpr (Soma a b) = (No (\x y -> x + y)) (fromExpr a) (fromExpr b)
fromExpr (Mult a b) = (No (\x y -> x * y)) (fromExpr a) (fromExpr b)
fromExpr (Div a b) = (No (\x y -> x `div` y)) (fromExpr a) (fromExpr b)
fromExpr (Sub a b) = (No (\x y -> x - y)) (fromExpr a) (fromExpr b)
fromExpr (Mod a b) = (No (\x y -> x `mod` y)) (fromExpr a) (fromExpr b)

eval :: ArvoreExpressao -> Int
eval (Folha a) = a
eval (No f a b) = f (eval a) (eval b)

showExpr :: Expr -> String
showExpr (Val a) = show a
showExpr (Soma a b) = "(" ++ (showExpr a) ++ " + " ++ (showExpr b) ++ ")"
showExpr (Mult a b) = "(" ++ (showExpr a) ++ " * " ++ (showExpr b) ++ ")"
showExpr (Div a b) = "(" ++ (showExpr a) ++ " / " ++ (showExpr b) ++ ")"
showExpr (Sub a b) = "(" ++ (showExpr a) ++ " - " ++ (showExpr b) ++ ")"
showExpr (Mod a b) = "(" ++ (showExpr a) ++ " % " ++ (showExpr b) ++ ")"