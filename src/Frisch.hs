module Frisch where

import Control.Monad
import Control.Monad.Identity ( Identity, runIdentity )
import Control.Monad.Trans.Class
import Control.Monad.Except
import Control.Monad.Signatures ( Catch )


type Name     = String
type NameList = [Name]

newtype FrischT m a = FrischT (NameList -> m (a,NameList))

type Frisch a = FrischT Identity a

instance Monad m => Functor (FrischT m)  where
    fmap f (FrischT fn) = FrischT $ \nl -> do 
                                             (a,nl') <- fn nl 
                                             return (f a,nl')


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

instance MonadTrans FrischT  where
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


-- liftCatch :: Catch e m a -> Catch e (FrischT r m) a
-- liftCatch f m h =
--     ReaderT $ \ r -> f (runReaderT m r) (\ e -> runReaderT (h e) r)

-- -- instantiation of mtl classes
-- instance MonadError e m => MonadError e (FrischT m a) where
--     throwError = lift . throwError
--     catchError = liftCatch catchError
