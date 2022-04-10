module Frisch where

import Control.Monad
import Control.Monad.Identity ( Identity )
import Control.Monad.Trans.Class


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
frisch = FrischT $ \nl -> case nl of 
                            (a:nl') -> return (a,nl')


-- runFrisch :: Frisch a -> NameList -> a
-- runFrisch (FrischT f Identity) list = fst (f list)

runFrischT :: Monad m => FrischT m a -> NameList -> m a
runFrischT (FrischT f) nl = do (a, _) <- f nl
                               return a
