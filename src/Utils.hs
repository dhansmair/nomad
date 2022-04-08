module Utils where

import Definitions

import Control.Monad.Trans.Except ( throwE ) 


makeApp :: Expr -> [Expr] -> Expr
makeApp = foldl App

makeAbs :: [String] -> Expr -> Expr 
makeAbs params ex = foldr Abs ex params

-- helper function which unwraps an either value or throws an exception
exceptify :: Monad m => Either MyError Expr -> MyException m Expr
exceptify eith = case eith of
    Right ex -> return ex
    Left err -> throwE err

op2str :: Op -> String
op2str Add = "#add"
op2str Sub = "#sub"
op2str Mul = "#mul"
op2str Div = "#div"
op2str Pow = "#pow"

str2op :: String -> Op
str2op "#add" = Add
str2op "#sub" = Sub
str2op "#mul" = Mul
str2op "#div" = Div
str2op "#pow" = Pow
str2op other = error $ other ++ "is not a valid operator name"

op2app :: Op -> Expr -> Expr -> Expr
op2app op a b = makeApp (Var (op2str op)) [a, b]

-- decompress :: Expr -> Expr
-- decompress (Abs (p1:p2:ps) ex) = Abs [p1] (Abs (p2:ps) (decompress ex))
-- decompress (Abs [p] ex) = Abs [p] ex
-- decompress (Abs [] ex) = ex
-- decompress (App ex args) = App (decompress ex) (map decompress args)
-- decompress (BinOp op a b) = BinOp op (decompress a) (decompress b)
-- decompress other = other

-- compress :: Expr -> Expr
-- compress (Abs ps1 (Abs ps2 ex)) = compress $ Abs (ps1 ++ ps2) ex
-- compress (App ex args) = App (compress ex) (map compress args)
-- compress (BinOp op a b) = BinOp op (compress a) (compress b)
-- compress other = other
