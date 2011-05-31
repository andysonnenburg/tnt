{-# LANGUAGE Rank2Types #-}
module Control.Monad.Nondet
       ( Nondet
       , NondetT
       , runNondet
       , runNondetT
       , option
       ) where
 
import Control.Monad
import Control.Monad.Trans
 
import Control.Monad.Identity
 
newtype NondetT m a = NondetT
                      { foldNondetT :: (forall b.
                                        (a -> m b -> m b) ->
                                        m b ->
                                        m b)
                      }
 
runNondetT :: (Monad m) => NondetT m a -> m a
runNondetT m = foldNondetT m (\x _xs -> return x) (error "No solution found.")
 
instance (Functor m) => Functor (NondetT m) where
  fmap f (NondetT g) = NondetT (\cons nil -> g (cons . f) nil)
 
instance (Monad m) => Monad (NondetT m) where
  return a = NondetT (\cons nil -> cons a nil)
  m >>= k  = NondetT (\cons nil ->
                       foldNondetT m (\x -> foldNondetT (k x) cons) nil)
 
instance (Monad m) => MonadPlus (NondetT m) where
  mzero         = NondetT (\_cons nil -> nil)
  m1 `mplus` m2 = NondetT (\cons -> foldNondetT m1 cons . foldNondetT m2 cons)
 
instance MonadTrans NondetT where
  lift m = NondetT (\cons nil -> m >>= \a -> cons a nil)
 
type Nondet = NondetT Identity

runNondet = runIdentity . runNondetT
 
option :: (MonadPlus m) => [a] -> m a
option = msum . map return