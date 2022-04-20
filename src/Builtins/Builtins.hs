{-
    Module to define all the builtin math stuff
-}

module Builtins.Builtins ( stdEnv, evalBuiltin ) where


import Definitions
import Environment
import Utils ( makeApp, makeAbs, tComb )
import Builtins.Simplification (simplify)
import Builtins.Derivation


type BuiltinFunc = [Expr] -> Either NomadError Expr

nanMsg = Left $ RuntimeError "builtin function requires input of type Num"
numArgMsg = Left $ RuntimeError "wrong number of arguments to [builtin]"


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

deriveB = Builtin $ B {name="derive", narg=1, func=deriveB', args=[]}
    where 
        deriveB' :: [Expr] -> Either NomadError Expr
        deriveB' [Abs [x] ex] = Right $ Abs [x] $ simplify $ deriveBy ex [x]
        deriveB' [Builtin b] = Right $ simplify $ deriveBy (Var (name b)) "x"
        deriveB' list = Left $ RuntimeError $ "cannot derive " ++ show list




-- some helper functions
--
makeUnary :: String -> (Double -> Double) -> Expr
makeUnary s f = Builtin $ B {name=s, narg=1, args=[], func=makeUnary' f}
    where
        makeUnary' :: (Double -> Double) -> BuiltinFunc
        makeUnary' f [Num x] = Right $ Num (f x)
        makeUnary' f [_] = nanMsg
        makeUnary' f _ = numArgMsg

makeBinary :: String -> (Double -> Double -> Double) -> Expr
makeBinary s f = Builtin $ B {name=s, narg=2, args=[], func=makeBinary' f}
    where
        makeBinary' :: (Double -> Double -> Double) -> BuiltinFunc
        makeBinary' f [Num a, Num b] = Right $ Num (f a b) 
        makeBinary' f [_, _] = nanMsg
        makeBinary' f _ = numArgMsg

makeTernary :: String -> (Double -> Double -> Double -> Double) -> Expr
makeTernary s f = Builtin $ B {name=s, narg=3, args=[], func=makeTernary' f}
    where
        makeTernary' :: (Double -> Double -> Double -> Double) -> BuiltinFunc
        makeTernary' f [Num a, Num b, Num c] = Right $ Num (f a b c) 
        makeTernary' f [_, _, _] = nanMsg
        makeTernary' f _ = numArgMsg


-- evaluate a Builtin with an argument.
-- if the parameters are saturated, execute the builtin, otherwise just store
-- the argument inside the builtin for later.
evalBuiltin :: Builtin -> Expr -> Either NomadError Expr
evalBuiltin b arg = do 
    if length (args b) == narg b - 1 then do
        -- apply the function
        let args' = reverse (arg : args b)  
         in func b args'

    else if length (args b) < narg b then do
        -- return a new builtin, but add the 
        return $ Builtin $ b {args=arg:args b}
    else do
        error "error in builtins, should not happen"
