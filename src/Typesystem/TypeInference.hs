{-# LANGUAGE FlexibleContexts #-}
{-
this Module exports the function 'getType', which determines the type for a 
given expression. 'getTypeExcept' does the same but returns an ExceptT Monad 
instead of an Either.
-}
module Typesystem.TypeInference ( getType, getTypeExcept ) where


import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.State

import Definitions
import Utils ( makeApp, op2app )
import Typesystem.TypeInferenceMonad
import Typesystem.Unification3
import Frisch


-- ex is the expression we want to type
-- gamma is the initial set of type assumptions, obtained from Env.
getType :: Expr -> [TypeAssumption] -> Either NomadError Type
getType ex assumptions = runExcept $ getTypeExcept ex assumptions

getTypeExcept :: Monad m => Expr -> [TypeAssumption] -> NomadExceptT m Type
getTypeExcept ex assumptions = do
    (t, equations) <- runTypeInferenceT (inferType ex) assumptions
    case mostGeneralUnifier t equations of 
        Just t2 -> return $ substituteSaneNames t2
        Nothing -> throwError $ TypeError "unification failed"


--------------------------------------------------------------------------------
-- inferType function
--------------------------------------------------------------------------------
inferType :: MonadError NomadError m => Expr -> TypeInferenceT m Type

-- AxV / AxSK:
-- type should be already determined -> Lookup in gamma
inferType (Var x) = do
    assumptions <- get
    case lookup x assumptions of
        Nothing -> throwError $ UndefinedVariableError x
        -- AxSK: if a variable is bound to an abstraction, it is a supercombinator.
        Just (TArr l r) -> lift $ substituteTypeVars $ TArr l r
        -- AxV: otherwise we have AxV, just return the type
        Just tvar -> return tvar

-- AxK: we only have one type of constructor, which has the type num
inferType (Num x) = return TNum

-- transform binary operators into applications of supercombinators.
-- the type (which is always num -> num -> num) is then looked up in gamma
inferType (BinOp op a b) = inferType $ op2app op a b 

-- RAbs: rule for abstractions
inferType (Abs s ex) = do
    alpha <- getName
    addAssumption (s,  alpha)
    t <- inferType ex
    return $ alpha @-> t

-- RApp: rule for application
inferType (App s t) = do
    tau1 <- inferType s 
    tau2 <- inferType t
    alpha <- getName
    addEquation (tau1, tau2 @-> alpha) 
    return alpha

-- this case should not happen, because the types of all builtins are in gamma
inferType (Builtin _) = error "trying to check type of builtin, this should not happen"



--------------------------------------------------------------------------------
-- helper functions
--------------------------------------------------------------------------------
(@->) :: Type -> Type -> Type
a @-> b = TArr a b
-- helper function that replaces type variables with easier to read ones,
-- i.e. replaces a1, a2, ... an 
--          with a, b, c, ... a2, b2, ...
substituteSaneNames :: Type -> Type
substituteSaneNames t = runFrisch (substituteTypeVars t) saneNames
  where
    saneChars = map (:[]) "abcdefghijklmnopqrstuvwxyz"
    saneNames = saneChars ++ [c ++ show n | n <- [2..], c <- saneChars]

-- used to give new names to all-quantified type assumptions in gamma,
-- and to subsitute type variable names with readable ones in the end.
substituteTypeVars :: (Monad m) => Type -> FrischT m Type
substituteTypeVars t = evalStateT (subst t) []
  where
    subst :: (Monad m) => Type -> StateT [(TypeId, TypeId)] (FrischT m) Type
    subst TNum = return TNum
    subst (TVar s) = do
        replacings <- get
        case lookup s replacings of
          Just n -> return $ TVar n
          Nothing -> do
              n <- lift frisch
              modify ((s, n):)
              return $ TVar n
    subst (TArr l r) = do
        l' <- subst l
        r' <- subst r
        return $ TArr l' r'

