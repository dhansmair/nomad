module Utils where

import Definitions

import Control.Monad.Trans.Except ( throwE ) 


tComb :: [Type] -> Type
tComb [] = error "Error, type list must not be empty"
tComb [x] = x
tComb (x:y:xs) = TArr x (tComb (y:xs))

makeApp :: Expr -> [Expr] -> Expr
makeApp = foldl App

makeAbs :: [String] -> Expr -> Expr 
makeAbs params ex = foldr Abs ex params

-- helper function which unwraps an either value or throws an exception
exceptify :: Monad m => Either NomadError Expr -> NomadExceptT m Expr
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

