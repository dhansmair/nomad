{-
this module exports only the evaluate function, and also the alphaRename
helper function, which needs to be done once in cli.hs, before the typeCheck 
can be performed.

alphaRename uses the strategy from the lecture script, but is implemented with
a state monad.

evaluate uses our universal Monad EnvT, where all currently available 
definitions (functions and variables) are stored.
evaluate always evaluates arguments before they are passed to abstractions.
It also supports partial evaluation, except for builtins.
evaluate of a Var fetches the value from EnvT, and then evaluates the value.
-}

module Interpreter( evaluate, alphaRename ) where

import Debug.Trace
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Identity 
import Control.Monad.Trans.State
import Definitions 
import Environment


evaluate :: Monad m => Expr -> MyException (EnvT m) Expr
evaluate (Num n) = return $ Num n
evaluate (Var s) = do
    valOrErr <- lift $ lookupValueT s
    val <- exceptify valOrErr
    evaluate val

evaluate (BinOp op e1 e2) = do
    n1 <- evaluate e1
    n2 <- evaluate e2
    exceptify $ applyBinOp op n1 n2

evaluate (Abs s ex) = return $ Abs s ex

evaluate (Builtin b) = return $ Builtin b
evaluate (App s t) = do
    s' <- evaluate s
    case s' of 
        Builtin b -> do
            t' <- evaluate t

            if length (args b) == narg b - 1 then do
                -- apply the function
                let args' = reverse (t' : args b)  
                exceptify $ func b args'
            else if length (args b) < narg b then do
                -- return a new builtin, but add the 
                return $ Builtin $ b {args=t':args b}
            else do
                error "error in builtins, should not happen"

        _ -> do
            t' <- evaluate t
            app' <- betaReduce (alphaRename s') t'
            evaluate app'

-- helper function which unwraps an either value or throws an exception
exceptify :: Monad m => Either MyError Expr -> MyException m Expr
exceptify eith = case eith of
    Right ex -> return ex
    Left err -> throwE err

-- betaReduce: small helper function for evaluate
-- intended to be only used on abstractions that still have remaining parameters
-- the RuntimeError would normally not occur here, this should already be cought 
-- by the type system
betaReduce :: Monad m =>  Expr -> Expr -> MyException m Expr
betaReduce (Abs p ex) arg = return $ replaceWith arg p ex
betaReduce other arg = throwE $ RuntimeError "Left side of application is not an abstraction"

-- replaceWith obj -> matcher -> body
replaceWith :: Expr -> String -> Expr -> Expr
replaceWith arg s (Num n) = Num n
replaceWith arg s (Var x) = if x == s then arg else Var x
replaceWith arg s (BinOp op a b) = 
    let a' = replaceWith arg s a
        b' = replaceWith arg s b
     in BinOp op a' b'
replaceWith arg s (Builtin b) = Builtin b
replaceWith arg s (App a b) = 
    let a' = replaceWith arg s a
        b' = replaceWith arg s b
     in App a' b'

-- important: need to make sure s is not in params, should be the case
-- because of previous alpha-renaming
replaceWith arg s (Abs p ex) = Abs p (replaceWith arg s ex) 


alphaRenameOld ex = evalState (alphaRenameM ex) ([], names)
    where
        names = map (\s -> 'x' : show s) (iterate (+1) 1)

        -- alpha renaming using a state Monad which stores a list of renamings
        -- (a,b) and a list of fresh variables
        alphaRenameM :: Expr -> State ([(String, String)], [String]) Expr
        alphaRenameM (Var v) = do
            (renamings, freshvars) <- get
            case lookup v renamings of
                Nothing -> return $ Var v
                Just v' -> return $ Var v'
        alphaRenameM (Num n) = return $ Num n
        alphaRenameM (BinOp op a b) = do
            a' <- alphaRenameM a
            b' <- alphaRenameM b
            return $ BinOp op a' b'
        alphaRenameM (App s t) = do
            s' <- alphaRenameM s
            t' <- alphaRenameM t
            return $ App s' t'
        alphaRenameM (Abs s ex) = do
            (renamings, freshvars) <- get
            let s' = head freshvars
                restvars = tail freshvars
            put ((s,s'):renamings, restvars)
            ex' <- alphaRenameM ex
            put (renamings, restvars) 
            return $ Abs s' ex'
        alphaRenameM (Builtin b) = return $ Builtin b


-- list for visited but not changed variables, and list for renamings.
-- whenever a new abstraction is encountered, it is checked whether the name
-- was visited but not changed.
-- If yes, a new replacing is introduced.
-- However, multiple replacings could occur.
-- In that case, the replacing must only happen within the scope of the variable.
type ARState = ([String], [(String, String)], [String])
alphaRename :: Expr -> Expr
alphaRename ex = evalState (ren ex) ([], [], names)
    where 
        names = map (\s -> '#' : show s) (iterate (+1) 1)

        ren :: Expr -> State ARState Expr
        ren (Var x) = do
            (v, renamings, nl) <- get
            case lookup x renamings of
                Nothing -> return $ Var x
                Just x' -> return $ Var x'
        ren (BinOp op a b) = do
            a' <- ren a
            b' <- ren b
            return $ BinOp op a' b'
        ren (App s t) = do
            s' <- ren s
            t' <- ren t
            return $ App s' t'

        ren (Abs s ex) = do
            (visited, renamings, nl) <- get

            if s `elem` visited then do
                -- introduce a new renaming
                let s' = head nl
                    nl' = tail nl
                put (visited, (s,s'):renamings, nl')
                ex' <- ren ex
                -- but pop it afterwards TODO is this necessary?
                put (visited, renamings, nl')

                return $ Abs s' ex'

            else do
                put (s:visited, renamings, nl)
                ex' <- ren ex
                return $ Abs s ex'

        ren other = return other

        checkVar :: String -> State ARState String
        checkVar s = do
            (visited, renamings, nl) <- get
            if s `elem` visited then do
                let s' = head nl
                put (visited, (s, s'):renamings, tail nl)
                return s'
            else do
                put (s:visited, renamings, nl)
                return s


-- helper function to evaluate binary operations on numbers
applyBinOp :: Op -> Expr -> Expr -> Either MyError Expr
applyBinOp op (Num a) (Num b) = Right $ Num $ applyBinOp' op a b
            where
                applyBinOp' Add a b = a + b
                applyBinOp' Sub a b = a - b
                applyBinOp' Mul a b = a * b
                applyBinOp' Div a b = a / b
                applyBinOp' Pow a b = a ** b

-- should be handled by the type system
applyBinOp op _ _ = Left $ RuntimeError $ "Tried to apply " ++ showEx op 
                            ++ " to wrong data types." 



