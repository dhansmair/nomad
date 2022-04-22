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


type TypeId = String
type VarId = String

type EnvDef = (VarId, Expr, Either NomadError Type, [VarId])
type Env = [EnvDef]
type EnvT = StateT Env

-- custom error type used throughout our program
data NomadError = BlankError String
             | ParseError String
             | TypeError String
             | RuntimeError String
             | CyclicDependencyError String
             | UndefinedVariableError String
             | InvalidCommandError String
             deriving(Eq)

type NomadExceptT = ExceptT NomadError

instance Show NomadError where
    show (BlankError s)   = "Error: " ++ s
    show (ParseError s)   = "Parse error: " ++ s
    show (TypeError s)    = "Type error: " ++ s
    show (RuntimeError s) = "Runtime error: " ++ s
    show (CyclicDependencyError s) = "Cyclic dependency error: " ++ s
    show (UndefinedVariableError s) = "Error: Undefined variable " ++ s
    show (InvalidCommandError s) = "Invalid command: " ++ s



-- TypeEquation is used in TypeCheck.hs and Unification.hs
type TypeEquation = (Type, Type)

-- Type: the type an expression can have. 
-- Features num, polymorphic type variables and abstractions.
data Type = TNum 
          | TVar TypeId 
          | TArr Type Type
    deriving(Eq)


instance Show Type where
    show TNum = "num"
    show (TVar s) = s
    show (TArr (TArr a b) c) = embrace (show (TArr a b)) ++ " -> " ++ show c
    show (TArr a b) = show a ++ " -> " ++ show b


-- Stmt and Expr are the two fundamental data declarations to which the user
-- input is parsed.
data Stmt = Def VarId Expr
          | Expr Expr
          deriving(Show, Eq)

data Expr = Num Double
          | Var VarId
          | BinOp Op Expr Expr
          | App Expr Expr 
          | Abs VarId Expr
          | Builtin Builtin
          deriving(Show, Eq)

data Op = Add | Sub | Mul | Div | Pow
    deriving(Show, Eq, Ord)

-- Buitin is a special type for Expr, which is used to provide a bridge between
-- our simple language and builtin functions
data Builtin = Unary   VarId (Expr -> Either NomadError Expr)
             | Binary  VarId (Expr -> Expr -> Either NomadError Expr)
             | Ternary VarId (Expr -> Expr -> Expr -> Either NomadError Expr)

instance Show Builtin where
    show (Unary s _)   = s
    show (Binary s _)  = s
    show (Ternary s _) = s

instance Eq Builtin where
    (==) (Unary s1 _)   (Unary s2 _)   = s1 == s2
    (==) (Binary s1 _)  (Binary s2 _)  = s1 == s2
    (==) (Ternary s1 _) (Ternary s2 _) = s1 == s2
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

    showEx (Builtin b) = show b

instance ShowEx Op where
    showEx Add = "+"
    showEx Sub = "-"
    showEx Mul = "*"
    showEx Div = "/" 
    showEx Pow = "^"


-- helper function for showEx
-- cannot put these in Utils since that would require a circular dependency
embrace :: String -> String
embrace s = "(" ++ s ++ ")"

-- only add parentheses to expressions if mathematically required, 
-- e.g. addition inside a multiplication 
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

