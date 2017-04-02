{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Structure.Fields where

import Data.Proxy
import Control.Structure
import Data.Structure.List
-------------------------------------------------------------------------------
data Field m t a = Field { getField :: t m -> m a, setField :: m a -> t m -> t m }

thisField :: Field m (a :> b) a
thisField = Field (\(ma :> bm) -> ma) (\ma (_ :> bm) -> (ma :> bm))

shiftField :: Field m b a -> Field m (x :> b) a
shiftField (Field get set) = Field (\(mx :> bm) -> get bm) (\ma (mx :> bm) -> mx :> set ma bm)

shiftField2 :: Field m b a -> Field m (x ::> b) a
shiftField2 (Field get set) = Field (\(mx ::> bm) -> get bm) (\ma (mx ::> bm) -> mx ::> set ma bm)

subField :: Field m b a -> Field m (b ::> x) a
subField (Field get set) = Field (\(mb ::> mx) -> get mb) (\ma (mb ::> mx) -> set ma mb ::> mx)

fields :: ToField t t => t (Field m t)
fields = toField Proxy
-------------------------------------------------------------------------------
class ToField t r where
  toField :: Proxy (t m) -> r (Field n t)

class PushField t where
  pushField :: t (Field m c) -> t (Field m (x :> c))

class PushField2 t where
  pushField2 :: t (Field m c) -> t (Field m (x ::> c))

class LiftField t where
  liftField :: t (Field m c) -> t (Field m (c ::> x))

--class Empty t where
--  emptyS :: t
-------------------------------------------------------------------------------
instance ToField NilM NilM where
  toField _ = NilM

instance (PushField b', ToField b b') => ToField (a :> b) (a :> b') where
  toField _ = thisField :> pushField (toField Proxy)

instance (LiftField a', ToField a a', PushField2 b', ToField b b') => ToField (a ::> b) (a' ::> b') where
  toField _ = liftField (toField Proxy) ::> pushField2 (toField Proxy)
-------------------------------------------------------------------------------
instance PushField NilM where
  pushField NilM = NilM

instance PushField b => PushField (a :> b) where
  pushField (ca :> bc) = shiftField ca :> pushField bc

instance (PushField a, PushField b) => PushField (a ::> b) where
  pushField (ac ::> bc) = pushField ac ::> pushField bc
-------------------------------------------------------------------------------
instance PushField2 NilM where
  pushField2 NilM = NilM

instance PushField2 b => PushField2 (a :> b) where
  pushField2 (ca :> bc) = shiftField2 ca :> pushField2 bc

instance (PushField2 a, PushField2 b) => PushField2 (a ::> b) where
  pushField2 (ac ::> bc) = pushField2 ac ::> pushField2 bc
-------------------------------------------------------------------------------
instance LiftField NilM where
  liftField NilM = NilM

instance LiftField b => LiftField (a :> b) where
  liftField (ma :> bm) = subField ma :> liftField bm

instance (LiftField a, LiftField b) => LiftField (a ::> b) where
  liftField (am ::> bm) = liftField am ::> liftField bm
-------------------------------------------------------------------------------
--instance Alternative m => Empty (NilM m) where
--  emptyS = NilM
--
--instance (Alternative m, Empty (b m)) => Empty ((a :> b) m) where
--  emptyS = Control.Applicative.empty :> emptyS
-------------------------------------------------------------------------------
--
--
--instance Monoid (m a) => Monoid (NilM m) where
--  mempty = NilM
--  mappend NilM NilM = NilM
--
--instance (Monoid (m a), Monoid (b m)) => Monoid ((a :> b) m) where
--  mempty = mempty :> mempty
--  mappend (ma1 :> bm1) (ma2 :> bm2) = (ma1 `mappend` ma2) :> (bm1 `mappend` bm2)
--
--instance (Monoid (a m), Monoid (b m)) => Monoid ((a ::> b) m) where
--  mempty = mempty ::> mempty
--  mappend (am1 ::> bm1) (am2 ::> bm2) = (am1 `mappend` am2) ::> (bm1 `mappend` bm2)
--
-------------------------------------------------------------------------------
