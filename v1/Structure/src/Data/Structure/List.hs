{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Data.Structure.List where

import Control.Structure
-------------------------------------------------------------------------------
data NilM (m :: * -> *) = NilM

data (a :> b) m = m a :> b m

data (u ::> b) (m :: * -> *) = u m ::> b m

infixr 6 :>
infixr 5 ::>

--getField (a :> b) = a
--dropField (a :> b) = b

instance Structure NilM where
    mapS f NilM = NilM
    appS NilM NilM = NilM
    seqS NilM = pure NilM

instance Structure b => Structure (a :> b) where
    mapS f (ma :> bm) = unFunc(unForall f) ma :> mapS f bm
    appS (ma1 :> bm1) (ma2 :> bm2) = (unFunc ma1 ma2) :> appS bm1 bm2
    seqS (ma :> bm) = (:>) <$> unComp ma <*> seqS bm


instance (Structure u, Structure b) => Structure (u ::> b) where
    mapS f (um ::> bm) = mapS f um ::> mapS f bm
    appS (um1 ::> bm1) (um2 ::> bm2) = appS um1 um2 ::> appS bm1 bm2
    seqS (um ::> bm) = (::>) <$> seqS um <*> seqS bm

-------------------------------------------------------------------------------
instance Show (NilM m) where
  showsPrec p x = showString "NilM\n"

instance (Show (m a), Show (b m)) => Show ((a :> b) m) where
  showsPrec p (ma :> bm) = showString (show ma ++ "\n" ++ show bm)

instance (Show (a m), Show (b m)) => Show ((a ::> b) m) where
  showsPrec p (am ::> bm) = showString (show am ++ show bm)

