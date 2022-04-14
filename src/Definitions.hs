{-
This module contains most of the type definitions used across our code, 
that are required in multiple places.
It has no dependencies to other modules, which is convenient because it can
be imported from everywhere without creating cyclic imports.
-}
module Definitions where

import Data.List (intercalate)
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Trans.State


type EnvDef = (String, Expr, Either NomadError Type, [String])
type Env = [EnvDef]
type EnvT = StateT Env

-- TODO define?
-- instance MonadError EnvT where

-- type NomadErrorCls m = MonadError NomadError m
type NomadExceptT = ExceptT NomadError

-- custom error type used throughout our program
data NomadError = BlankError String
             | ParseError String
             | TypeError String
             | RuntimeError String
             | CyclicDependencyError String
             | UndefinedVariableError String
             | InvalidCommandError String

instance Show NomadError where
    show (BlankError s)   = "Error: " ++ s
    show (ParseError s)   = "Parse error: " ++ s
    show (TypeError s)    = "Type error: " ++ s
    show (RuntimeError s) = "Runtime error: " ++ s
    show (CyclicDependencyError s) = "Cyclic dependency error: " ++ s
    show (UndefinedVariableError s) = "Undefined variable error: " ++ s
    show (InvalidCommandError s) = "Invalid command: " ++ s


-- TypeEquation is used in TypeCheck.hs and Unification.hs
type TypeEquation = (Type, Type)

-- Type: the type an expression can have. 
-- Features num, polymorphic type variables and abstractions.
data Type = TNum 
          | TVar String 
          | TArr Type Type
    deriving(Eq)


instance Show Type where
    show TNum = "num"
    show (TVar s) = s
    show (TArr (TArr a b) c) = embrace (show (TArr a b)) ++ " -> " ++ show c
    show (TArr a b) = show a ++ " -> " ++ show b


-- Stmt and Expr are the two fundamental data declarations to which the user
-- input is parsed.
--
type Id = String
--
--
data Stmt = Def Id Expr
          | Expr Expr
          deriving(Show)

data Expr = Num Double
          | Var Id
          | BinOp Op Expr Expr
          | App Expr Expr 
          | Abs Id Expr
          | Builtin Builtin 
          deriving(Show)

data Op = Add | Sub | Mul | Div | Pow
    deriving(Show, Eq, Ord)

-- Buitin is a special type for Expr, which is used to provide a bridge between
-- our simple language and builtin functions
data Builtin = B { narg :: Int
                 , args :: [Expr]
                 , func :: [Expr] -> Either NomadError Expr
                 , name :: String
                 }
instance Show Builtin where
    show (B _ [] _ name) = name
    show (B n args _ name) 
      | length args < n = let qs = intercalate "," (replicate (n - length args) "?")
                           in name ++ "(" ++ pArgs args ++ ", " ++ qs ++ ")"
      | length args == n = name ++ "(" ++ pArgs args ++ ")"



instance Eq Expr where
    (==) (Num a) (Num b) = a == b
    (==) (Var x) (Var y) = x == y 
    (==) (BinOp op1 a1 b1) (BinOp op2 a2 b2) = op1 == op2 && a1 == a2 && b1 == b2
    (==) (App ex1 l1) (App ex2 l2) = ex1 == ex2 && l1 == l2
    (==) (Abs l1 ex1) (Abs l2 ex2) = False
    (==) (Builtin b1) (Builtin b2) = name b1 == name b2
    (==) _ _ = False
    

-- showEx: special class for our Expressions. The showEx function is used to
-- 'prettyprint' Expressions for the user, with the same Syntax as if it was 
-- typed in (outputs legal strings that could be parsed again with our grammar. Used 'historically', because show was used for debugging.
-- Probably we could replace ShowEx with Show now.
class ShowEx a where
    showEx :: a -> String

instance ShowEx Expr where
    showEx (Num d) = show d
    showEx (Var s) = s
    showEx (BinOp op a b) = embraceIfLower op a ++ " " 
                            ++ showEx op ++ " " 
                            ++ embraceIfLower op b

    showEx (App s t) = 
        let (s', args) = stretchApp (App s t)
         in helper s' ++ embrace (pArgs args)
        where helper :: Expr -> String
              helper (Var s) = s
              helper other   = embrace $ showEx other

    showEx (Abs x ex) = 
        let (args, ex') = squeezeAbs (Abs x ex)
         in '\\' : intercalate ", " args ++ " -> " ++ showEx ex'

    showEx (Builtin b) = name b

instance ShowEx Op where
    showEx Add = "+"
    showEx Sub = "-"
    showEx Mul = "*"
    showEx Div = "/" 
    showEx Pow = "^"


-- helper function for showEx
embrace :: String -> String
embrace s = "(" ++ s ++ ")"

embraceIfLower :: Op -> Expr -> String
embraceIfLower op1 (BinOp op2 a b)
    | op1 > op2 = embrace $ showEx $ BinOp op2 a b
    | otherwise = showEx $ BinOp op2 a b
embraceIfLower op1 other = showEx other

pArgs :: [Expr] -> String
pArgs list = intercalate ", " $ map showEx list

squeezeAbs :: Expr -> ([String], Expr)
squeezeAbs (Abs s ex) =
    let (l, ex') = squeezeAbs ex
     in (s:l, ex')
squeezeAbs other = ([], other)

stretchApp :: Expr -> (Expr, [Expr])
stretchApp (App s t) = 
    let (s', args) = stretchApp s
     in (s', args ++ [t])

stretchApp other = (other, [])

