module Builtins.Derivation ( deriveBy ) where


import Definitions

(#+) = BinOp Add 
(#-) = BinOp Sub
(#*) = BinOp Mul
(#/) = BinOp Div
(#^) = BinOp Pow


deriveBy :: Expr -> String -> Expr
deriveBy (Num n) s = Num 0
deriveBy (BinOp Add a b) s = deriveBy a s #+ deriveBy b s 
deriveBy (BinOp Sub a b) s = deriveBy a s #- deriveBy b s
deriveBy (BinOp Mul u v) s = (deriveBy u s #* v) #+ (u #* deriveBy v s)
deriveBy (BinOp Div u v) s = ((deriveBy u s #* v) #- (u #* deriveBy v s)) #/ (v #^ Num 2)
deriveBy (BinOp Pow f g) s = (f #^ g) #* (l #+ r)
    where
        l = deriveBy g s #* App (Var "ln") f
        r = deriveBy f s #* (g #/ f)

deriveBy (Var "sin") _ = Var "cos"
deriveBy (Var "cos") _ = Abs "x" $ Num (-1) #* App (Var "sin") (Var "x")
deriveBy (Var "tan") _ = Abs "x" $ Num 1 #/ App (Var "cos") (Var "x") #^ Num 2
deriveBy (Var "ln" ) _ = Abs "x" $ Num 1 #/ Var "x"
deriveBy (Var "exp") _ = Var "exp"
deriveBy (Var "abs") _ = error "cannot derive abs"
deriveBy (Var "max") _ = error "cannot derive max"

deriveBy (Var x)     s = if x == s then Num 1 else Num 0

-- chain rule, nachdifferenzieren
-- deriveBy (App (Var f) [x]) s = App (deriveBy (Var f) s) [x] #* deriveBy x s
-- lazy evaluation
deriveBy (App (Var f) x) s = App (App (Var "derive") (Var f)) x #* deriveBy x s

deriveBy ex s = error $ "cannot derive " ++ show ex 

