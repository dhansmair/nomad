{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}

module Frisch where

import Control.Monad
import Control.Monad.Identity ( Identity, runIdentity )
import Control.Monad.Trans.Class
import Control.Monad.Writer.Class
import Control.Monad.Except
import Control.Monad.Signatures ( Catch )
-- import Definitions (NomadError)


type Name     = String
type NameList = [Name]

newtype FrischT m a = FrischT (NameList -> m (a, NameList))

type Frisch a = FrischT Identity a

instance Monad m => Functor (FrischT m)  where
    fmap f (FrischT fn) = FrischT $ \nl -> do 
                                             (a,nl') <- fn nl 
                                             return (f a, nl')


instance Monad m => Applicative (FrischT m) where
    pure = return
    -- (<*>) :: FrischT m (a -> b) -> FrischT m a -> FrischT m b
    f <*> x = do
        f' <- f
        x' <- x
        return (f' x')
                
instance Monad m => Monad (FrischT m)  where
    return x = FrischT $ \nl -> return (x,nl)
    (FrischT fn) >>= f = FrischT $ \nl -> do 
                                            (a,nl') <- fn nl 
                                            case f a of 
                                                FrischT fn' -> fn' nl'

instance MonadTrans FrischT where
    -- lift :: Monad m => m a -> FrischT m a
    lift act = FrischT $ \nl -> do
                                 x <- act
                                 return (x,nl)

frisch :: Monad m => FrischT m Name
frisch = FrischT $ \(a:nl) -> return (a, nl)

runFrisch :: Frisch a -> NameList -> a
runFrisch (FrischT f) nl = fst $ runIdentity $ f nl

runFrischT :: Monad m => FrischT m a -> NameList -> m a
runFrischT (FrischT f) nl = do (a, _) <- f nl
                               return a


-- liftCatch :: Catch e m a -> Catch e (FrischT m) a
-- liftCatch f m h =
--     FrischT $ \ a -> (f (runFrischT m a) (\e -> runFrischT (h e) a)

-- -- instantiation of mtl classes
instance (MonadError e m) => MonadError e (FrischT m) where
    throwError = lift . throwError
    -- catchError = liftCatch catchError
    --
instance MonadWriter w m => MonadWriter w (FrischT m) where
    writer = lift . writer
    tell   = lift . tell
    -- listen = mapReaderT listen
    -- pass   = mapReaderT pass
