{-# LANGUAGE FlexibleContexts #-}
{-
this Module exports the function 'getType', which determines the type for a 
given expression.

The typecheck for our language is simpler than for haskell:
- only one data type: num
- no cyclic function calls: Since we do not have if/else, we cannot stop recursions,
  meaning cyclic function calls must be prohibited alltogether. 
  (during variable/function definition, we check for cycles)
  Thus we also don't need to type recursive supercombinators.

General strategy:
    in getType, first the type and a list of equations are determined using getTypeM.
    Then the most general unifier is computed from the equations.
    Lastly, the type variables are renamed to more readable names (a, b, c, ..)

Error cases:
    The typecheck fails if the expression cannot be typed, or if a variable is 
    not defined.

Disclaimer:
    There are no useful type error messages. The program only states that there
    is a type error.
    This would be a useful feature to implement in the future.

-}

module TypeCheck ( getType
                 , getTypePure
                 , getTypeEither ) where


import Control.Monad
import Control.Monad.Writer.Class ( MonadWriter )
import Control.Monad.State.Class ( MonadState )
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Maybe ( mapMaybe )
import Data.List ( intercalate )

import Definitions
import Utils ( makeApp, op2app )
import Unification2 ( unify )
import Frisch

type TypeAssumption = (String, Type)

-- custom monad TypeInferenceT:
-- StateT [TypeAssumption]      initial assumptions (gamma)
-- FrischT                      contains fresh variable names
-- WriterT                      collects type equations for later unification
type TypeInferenceT m = StateT [TypeAssumption] 
                      ( FrischT 
                      ( WriterT [TypeEquation] 
                        m))

names = ['a': show s | s <- [1..]]

(@->) :: Type -> Type -> Type
a @-> b = TArr a b

-- the main function of this module.
-- parameters:
-- ex is the expression we want to type
-- gamma0 is the initial set of type assumptions, obtained from Env.
--  it contains the types for all builtins and user-defined functions.
-- returns Left String in case of failure with an error message, 
-- or Right type in case of success.
--
-- getType internally calls the monadic function getTypeM
getType :: (Monad m) => Expr -> NomadExceptT (EnvT m) Type
getType ex = do
    gamma0 <- lift getInitialAssumptions
    (t, equations) <- runWriterT $ runFrischT (evalStateT (getTypeM ex) gamma0) names 

    case unify t equations of 
        (Just t2) -> return $ substituteSaneNames t2
        Nothing -> throwError $ TypeError "unification failed"


-- this is a non-Monadic version of getType. 
-- It is required in some functions that do not know the EnvT monad
getTypePure :: Expr -> [TypeAssumption] -> Either NomadError Type
getTypePure ex gamma0 = do
    res <- runExceptT $ runWriterT $ runFrischT (evalStateT (getTypeM ex) gamma0) names

    case res of
        Left err -> Left err
        Right (t, equations) -> 
            case unify t equations of 
                Just t' -> Right $ substituteSaneNames t'
                Nothing -> Left $ TypeError "unification failed"


-- another version of getType that returns an Either instead of throwing 
-- an exception
getTypeEither :: (Monad m) => Expr -> (EnvT m) (Either NomadError Type)
getTypeEither ex = runExceptT (getType ex)


-- helper function to extract the initial type assumptions from EnvT
getInitialAssumptions :: (Monad m) => EnvT m [TypeAssumption]
getInitialAssumptions = do mapMaybe helper <$> get
    where
        helper (s, _, Right t, _) = Just (s, t)
        helper (_, _, Left _, _) = Nothing

-- helper functions to easier access and modify the state monad
-- get new type variable with a fresh name
getName :: (Monad m) => TypeInferenceT m Type
getName = do
    n <- lift frisch
    return $ TVar n 

-- add a new type assumption to the state
addAssumption :: (Monad m) => TypeAssumption -> TypeInferenceT m ()
addAssumption g = modify (g:)

-- add a new type equation to the state
addEquation :: (Monad m) => TypeEquation -> TypeInferenceT m ()
addEquation eq = lift $ lift $ tell [eq]
    
-- AxV / AxSK:
-- type should be already determined -> Lookup in gamma
getTypeM :: (MonadError NomadError m) => Expr -> TypeInferenceT m Type
getTypeM (Var x) = do
    assumptions <- get
    case lookup x assumptions of
        Nothing -> throwError $ UndefinedVariableError x
        Just t -> do
            case t of
                -- AxSK: if a variable is bound to an abstraction, it is a supercombinator.
                TArr _ _ -> lift $ substituteTypeVars t
                -- AxV: otherwise we have AxV, just return the type
                _ -> return t

-- AxK: we only have one type of constructor, which has the type num
getTypeM (Num x) = return TNum

-- transform binary operators into applications of supercombinators.
-- the type (which is always num -> num -> num) is then looked up in gamma
getTypeM (BinOp op a b) = getTypeM $ op2app op a b 

-- RAbs: rule for abstractions
getTypeM (Abs s ex) = do
    alpha <- getName
    addAssumption (s,  alpha)
    t <- getTypeM ex
    return $ alpha @-> t

-- RApp: rule for application
getTypeM (App s t) = do
    tau1 <- getTypeM s 
    tau2 <- getTypeM t
    alpha <- getName
    addEquation (tau1, tau2 @-> alpha) 
    return alpha


-- this case should not happen, because the types of all builtins are in gamma0
-- and thus simpliy looked up
getTypeM (Builtin _) = error "trying to check type of builtin, this should not happen"


-- helper function for the renaming of variables.
-- used to give new names to all-quantified type assumptions in gamma,
-- and to subsitute type variable names with readable ones in the end.
substituteTypeVars :: (Monad m) => Type -> FrischT m Type
substituteTypeVars t = evalStateT (subst t) []
    where
        subst :: (Monad m) => Type -> StateT [(String, String)] (FrischT m) Type
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

-- helper function that replaces type variables with easier to read ones,
-- i.e. replaces a1, a2, ... an 
--          with a, b, c, ... a2, b2, ...
substituteSaneNames :: Type -> Type
substituteSaneNames t = runFrisch (substituteTypeVars t) saneNames
    where
        saneChars = map (:[]) "abcdefghijklmnopqrstuvwxyz"
        saneNames = saneChars ++ [c ++ show n | n <- [2..], c <- saneChars]
