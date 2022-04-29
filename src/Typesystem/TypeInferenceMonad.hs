module Typesystem.TypeInferenceMonad where

import Control.Monad
import Control.Monad.Trans ( lift )
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import Frisch
import Definitions


type TypeAssumption = (TypeId, Type)

-- custom monad TypeInferenceT:
-- StateT [TypeAssumption]      initial assumptions (gamma)
-- FrischT                      contains fresh variable names
-- WriterT                      collects type equations for later unification
type TypeInferenceT m = StateT [TypeAssumption] 
                      ( FrischT 
                      ( WriterT [TypeEquation] 
                        m))


runTypeInferenceT :: Monad m => TypeInferenceT m a -> [TypeAssumption] -> m (a, [TypeEquation])
runTypeInferenceT tInfT gamma = do
    runWriterT $ runFrischT (evalStateT tInfT gamma) names
        where names = ['a': show s | s <- [1..]]


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
    
