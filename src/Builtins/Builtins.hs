{-
    Module to define all the builtin math stuff
-}

module Builtins.Builtins ( stdEnv, evalBuiltin ) where


import Definitions
import Environment
import Utils ( makeApp, makeAbs, tComb )
import Builtins.Simplification (simplify)
import Builtins.Derivation


nanMsg = Left $ RuntimeError "builtin function requires input of type Num"

-- create the environment,
-- including standard functions etc
stdEnv :: Env
stdEnv = 
    [ 
    -- # functions are internals that cannot be used directly from the command line.
    -- Those are needed for the typecheck of the binary operations.
      ("#add", addB,       Right (tComb [TNum, TNum, TNum]), [])
    , ("#sub", subB,       Right (tComb [TNum, TNum, TNum]), [])
    , ("#mul", mulB,       Right (tComb [TNum, TNum, TNum]), [])
    , ("#div", divB,       Right (tComb [TNum, TNum, TNum]), [])
    , ("#pow", powB,       Right (tComb [TNum, TNum, TNum]), [])

    -- default library functions
    , ("sin", sinB,        Right (tComb [TNum, TNum]), [])
    , ("cos", cosB,        Right (tComb [TNum, TNum]), [])
    , ("tan", tanB,        Right (tComb [TNum, TNum]), [])
    , ("exp", expB,        Right (tComb [TNum, TNum]), [])
    , ("ln" , lnB,         Right (tComb [TNum, TNum]), [])
    , ("log" , logB,       Right (tComb [TNum, TNum, TNum]), [])
    , ("root", rootB,      Right (tComb [TNum, TNum, TNum]), [])
    , ("sqrt", sqrtB,      Right (tComb [TNum, TNum]), [])

    -- other
    , ("abs", absB,        Right (tComb [TNum, TNum]), [])
    , ("max", maxB,        Right (tComb [TNum, TNum, TNum]), [])

    -- constants
    , ("e"  , Num (exp 1), Right TNum, [])
    , ("pi" , Num pi,      Right TNum, [])

    , ("derive", deriveB,  Right (tComb [tComb [TNum, TNum], tComb [TNum, TNum]]), [])
    , ("id"    , idB,      Right (tComb [TVar "a", TVar "a"]), [])
    ]


-- builtin functions
--
idB = Abs "x" (Var "x")
addB = makeBinary "add" (+)
subB = makeBinary "sub" (-)
mulB = makeBinary "mul" (*)
divB = makeBinary "div" (/)
powB = makeBinary "pow" (**)

absB = makeUnary "abs" abs
sinB = makeUnary "sin" sin
cosB = makeUnary "cos" cos
tanB = makeUnary "tan" tan
expB = makeUnary "exp" exp
lnB  = makeUnary "ln"  log
logB = makeBinary "log" logBase
maxB = makeBinary "max" max

rootB = makeAbs ["n", "x"] (BinOp Pow (Var "x") (BinOp Div (Num 1) (Var "n")))
sqrtB = makeAbs ["x"] (BinOp Pow (Var "x") (Num 0.5))

deriveB = Builtin $ Unary "derive" deriveB'
    where 
        deriveB' :: Expr -> Either NomadError Expr
        deriveB' (Abs [x] ex) = Right $ Abs [x] $ simplify $ deriveBy ex [x]
        deriveB' (Builtin b) = Right $ simplify $ deriveBy (Var (getName b)) "x"
        deriveB' list = Left $ RuntimeError $ "cannot derive " ++ show list

        getName :: Builtin -> String
        getName (Unary s _)   = s
        getName (Binary s _)  = s
        getName (Ternary s _) = s

-- some helper functions
--
makeUnary :: String -> (Double -> Double) -> Expr
makeUnary s f = Builtin $ Unary s (helper f)
    where helper :: (Double -> Double) -> (Expr -> Either NomadError Expr)
          helper f (Num x) = Right $ Num (f x)
          helper f _ = nanMsg

makeBinary :: String -> (Double -> Double -> Double) -> Expr
makeBinary s f = Builtin $ Binary s (helper f)
    where helper :: (Double -> Double -> Double) -> (Expr -> Expr -> Either NomadError Expr)
          helper f (Num x) (Num y) = Right $ Num (f x y)
          helper f _ _ = nanMsg

makeTernary :: String -> (Double -> Double -> Double -> Double) -> Expr
makeTernary s f = Builtin $ Ternary s (helper f)
    where helper :: (Double -> Double -> Double -> Double) -> (Expr -> Expr -> Expr -> Either NomadError Expr)
          helper f (Num x) (Num y) (Num z) = Right $ Num (f x y z)
          helper f _ _ _ = nanMsg

-- evaluate a Builtin with an argument.
-- if the parameters are saturated, execute the builtin, otherwise just store
-- the argument inside the builtin for later.
evalBuiltin :: Builtin -> Expr -> Either NomadError Expr
evalBuiltin (Unary s f)   arg = f arg
evalBuiltin (Binary s f)  arg = Right $ Builtin $ Unary s (f arg)
evalBuiltin (Ternary s f) arg = Right $ Builtin $ Binary s (f arg)
