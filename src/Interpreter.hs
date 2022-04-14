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
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Identity 
import Control.Monad.Trans.State
import Definitions 
import Environment
import Builtins ( evalBuiltin )
import Utils ( exceptify, makeApp, op2app )
import Frisch


evaluate :: Monad m => Expr -> NomadExceptT (EnvT m) Expr
evaluate (Var s) = do
    res <- lift $ lookupVar s
    case res of
        Nothing -> throwError $ UndefinedVariableError s 
        Just ex -> evaluate ex
evaluate (BinOp op a b) = evaluate $ op2app op a b
evaluate (App s t) = do
    s' <- evaluate s
    case s' of 
        Builtin b -> do
            t' <- evaluate t
            exceptify $ evalBuiltin b t'
        Abs param ex -> do
            t' <- evaluate t
            evaluate $ replaceWith t' param ex
        _ -> error "Left side of App is not an Abs, should have been prevented by the type system. It is a bug in Nomad"
evaluate other = return other


-- TODO ordering of arguments is not intuitive?
-- betaReduce (App (Abs s ex) arg) -> s ex arg
-- notation from the lecture: ex[arg/s]
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


alphaRename :: Expr -> Expr
alphaRename ex = evalState (runFrischT (runReaderT (ren ex) []) names) []
    where 
        names = ['#':show i | i <- [1..]]

        -- TODO why does this not work?
        -- ren :: Expr -> ReaderT [(String, String)] (StateT [String] (Frisch String)) Expr
        ren :: Expr -> ReaderT [(String, String)] (FrischT (State [String])) Expr
        ren (Var x) = do
            renamings <- ask
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
            visited <- lift $ lift get
            if s `elem` visited then do
                s' <- lift frisch
                ex' <- local ((s, s'):) (trace ("frisch: " ++ show s') (ren ex))
                return $ Abs s' ex'
            else do
                lift $ lift $ put (s:visited)
                ex' <- ren ex
                return $ Abs s ex'
        ren other = return other

-- alphaRenameOld ex = evalState (alphaRenameM ex) ([], names)
--     where
--         names = map (\s -> 'x' : show s) (iterate (+1) 1)

--         -- alpha renaming using a state Monad which stores a list of renamings
--         -- (a,b) and a list of fresh variables
--         alphaRenameM :: Expr -> State ([(String, String)], [String]) Expr
--         alphaRenameM (Var v) = do
--             (renamings, freshvars) <- get
--             case lookup v renamings of
--                 Nothing -> return $ Var v
--                 Just v' -> return $ Var v'
--         alphaRenameM (Num n) = return $ Num n
--         alphaRenameM (BinOp op a b) = do
--             a' <- alphaRenameM a
--             b' <- alphaRenameM b
--             return $ BinOp op a' b'
--         alphaRenameM (App s t) = do
--             s' <- alphaRenameM s
--             t' <- alphaRenameM t
--             return $ App s' t'
--         alphaRenameM (Abs s ex) = do
--             (renamings, freshvars) <- get
--             let s' = head freshvars
--                 restvars = tail freshvars
--             put ((s,s'):renamings, restvars)
--             ex' <- alphaRenameM ex
--             put (renamings, restvars) 
--             return $ Abs s' ex'
--         alphaRenameM (Builtin b) = return $ Builtin b
