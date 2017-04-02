{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

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
class FunctorS (t :: (* -> *) -> *) where
  mapS :: Forall (Func m n) -> t m -> t n

class ApplicativeS (t1 :: (* -> *) -> *) (t2 :: (* -> *) -> *) where
  appS :: t1 (Func m n) -> t2 m -> t2 n

class TraversableS (t :: (* -> *) -> *) where
  seqS :: Applicative m => t (Comp m n) -> m (t n)

class ParseS (t1 :: (* -> *) -> *) (t2 :: (* -> *) -> *) where
  parseS :: Alternative m => t1 m -> m (t2 Identity)
-------------------------------------------------------------------------------
data Product (a :: (* -> *) -> *) (b :: (* -> *) -> *) (m :: * -> *)
  = Product (a m) (b m)
  deriving Show

instance (FunctorS t1, FunctorS t2) => FunctorS (Product t1 t2) where
  mapS f (Product t1 t2) = Product (mapS f t1) (mapS f t2)

instance (ApplicativeS t1 t1, ApplicativeS t2 t2) => ApplicativeS (Product t1 t2) (Product t1 t2) where
  appS (Product f1 f2) (Product t1 t2) = Product (appS f1 t1) (appS f2 t2)

instance (TraversableS t1, TraversableS t2) => TraversableS (Product t1 t2) where
  seqS (Product t1 t2) = Product <$> seqS t1 <*> seqS t2

instance (ParseS t1 t1', ParseS t2 t2') => ParseS (Product t1 t2) (Sum t1' t2') where
  parseS (Product t1 t2) = (LeftS <$> parseS t1) <|> (RightS <$> parseS t2)
-------------------------------------------------------------------------------
data Sum (a :: (* -> *) -> *) (b :: (* -> *) -> *) (m :: * -> *)
  = LeftS (a m)
  | RightS (b m)
  deriving Show

instance (FunctorS t1, FunctorS t2) => FunctorS (Sum t1 t2) where
  mapS f (LeftS t1) = LeftS (mapS f t1)
  mapS f (RightS t2) = RightS (mapS f t2)

instance (ApplicativeS t1 t1, ApplicativeS t2 t2) => ApplicativeS (Product t1 t2) (Sum t1 t2) where
  appS (Product f1 f2) (LeftS t1) = LeftS (appS f1 t1)
  appS (Product f1 f2) (RightS t2) = RightS (appS f2 t2)

instance (TraversableS t1, TraversableS t2) => TraversableS (Sum t1 t2) where
  seqS (LeftS t1) = LeftS <$> seqS t1
  seqS (RightS t2) = RightS <$> seqS t2
-------------------------------------------------------------------------------
readS :: (TraversableS t, FunctorS t, Monad m) => t (ReaderT i m) -> Reader i (t m)
readS t = seqS $ mapS (Forall (Func $ \r -> Comp (ask >>= \x -> return $ runReaderT r x))) t
---------------------------------------------------------------------------------
runStateS :: (FunctorS t, TraversableS t, Monad m) => t (StateT acc m) -> StateT acc m (t Identity)
runStateS = seqS . mapS (Forall (Func (\s -> Comp (fmap Identity s))))

toIdentity :: (Applicative m, FunctorS t, TraversableS t) => t m -> m (t Identity)
toIdentity = seqS . mapS (Forall (Func $ \mb -> Comp (fmap Identity mb)))

toEmpty :: (Alternative n, FunctorS t) => t (m :: * -> *) -> t n
toEmpty = mapS $ Forall $ Func $ \_ -> Control.Applicative.empty

zipWithS :: (FunctorS t1, ApplicativeS t1 t2) => Forall (Func m (Func n o)) -> t1 m -> t2 n -> t2 o
zipWithS f t1 t2 = appS (mapS f t1) t2

mapConstS :: FunctorS t => (a -> b) -> t (Const a) -> t (Const b)
mapConstS f = mapS (Forall $ Func $ \(Const a) -> Const (f a))

duplicateS :: (Applicative m, FunctorS t) => t m -> t (Comp m m)
duplicateS = mapS (Forall (Func (\ma -> Comp (pure <$> ma))))

extendS :: (Functor m, Applicative n, FunctorS t) => t m -> t (Comp m n)
extendS = mapS (Forall (Func (\ma -> Comp (pure <$> ma))))

composeS :: (FunctorS t1, ApplicativeS t1 t2) => t1 (Func m n) -> t2 (Func n o) -> t2 (Func m o)
composeS t1 t2 =
  appS
    (mapS
      (Forall (Func $ \fmn -> Func $ \fno -> Func $ \ma -> unFunc fno (unFunc fmn ma)))
      t1
    )
    t2
---------------------------------------------------------------------------------
type FoldF m state = Func Identity (StateT state m)

foldS :: (Monad m, ApplicativeS t1 t2, FunctorS t2, TraversableS t2) => t1 (FoldF m acc) -> t2 Identity -> StateT acc m (t2 Identity)
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
