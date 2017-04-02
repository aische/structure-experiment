{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Control.Structure where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor.Identity
-------------------------------------------------------------------------------
data Func m n a = Func { unFunc :: m a -> n a }

data Comp m n a = Comp { unComp :: m (n a) }

data Forall m = Forall { unForall :: forall a . m a }
-------------------------------------------------------------------------------
class Structure (t :: (* -> *) -> *) where
  mapS :: Forall (Func m n) -> t m -> t n
  appS :: t (Func m n) -> t m -> t n
  seqS :: Applicative m => t (Comp m n) -> m (t n)
-------------------------------------------------------------------------------
readS :: (Structure t, Monad m) => t (ReaderT i m) -> Reader i (t m)
readS t = seqS $ mapS (Forall (Func $ \r -> Comp (ask >>= \x -> return $ runReaderT r x))) t
-------------------------------------------------------------------------------
runStateS :: (Structure t, Monad m) => t (StateT acc m) -> StateT acc m (t Identity)
runStateS = seqS . mapS (Forall (Func (\s -> Comp (fmap Identity s))))

toIdentity :: (Applicative m, Structure t) => t m -> m (t Identity)
toIdentity = seqS . mapS (Forall (Func $ \mb -> Comp (fmap Identity mb)))

toEmpty :: (Alternative n, Structure t) => t (m :: * -> *) -> t n
toEmpty = mapS $ Forall $ Func $ \_ -> Control.Applicative.empty

zipWithS :: Structure t => Forall (Func m (Func n o)) -> t m -> t n -> t o
zipWithS f t1 t2 = appS (mapS f t1) t2

mapConstS :: Structure t => (a -> b) -> t (Const a) -> t (Const b)
mapConstS f = mapS (Forall $ Func $ \(Const a) -> Const (f a))

duplicateS :: (Applicative m, Structure t) => t m -> t (Comp m m)
duplicateS = mapS (Forall (Func (\ma -> Comp (pure <$> ma))))

extendS :: (Functor m, Applicative n, Structure t) => t m -> t (Comp m n)
extendS = mapS (Forall (Func (\ma -> Comp (pure <$> ma))))

composeS :: Structure t => t (Func m n) -> t (Func n o) -> t (Func m o)
composeS t1 t2 =
  appS
    (mapS
      (Forall (Func $ \fmn -> Func $ \fno -> Func $ \ma -> unFunc fno (unFunc fmn ma)))
      t1
    )
    t2

-------------------------------------------------------------------------------
type FoldF m state = Func Identity (StateT state m)

foldS :: (Monad m, Structure t) => t (FoldF m acc) -> t Identity -> StateT acc m (t Identity)
foldS tf ti = runStateS (appS tf ti)

foldFIgnore :: Monad m => FoldF m a b
foldFIgnore = Func $ \(Identity x) -> pure x

foldFState :: Monad m => (a -> b -> (b, a)) -> FoldF m a b
foldFState f = Func $ \(Identity x) -> state $ \s -> f s x

foldFStateM :: Monad m => (a -> b -> m (b, a)) -> FoldF m a b
foldFStateM f = Func $ \(Identity x) -> StateT $ \s -> f s x

foldFWrite :: Monad m => (a -> b -> a) -> FoldF m a b
foldFWrite f = Func $ \(Identity x) -> state $ \s -> (x, f s x)

foldFRead :: Monad m => (a -> b -> b) -> FoldF m a b
foldFRead f = Func $ \(Identity x) -> state $ \s -> (f s x, s)
-------------------------------------------------------------------------------
