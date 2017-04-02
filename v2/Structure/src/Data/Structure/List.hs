{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Structure.List where

import Control.Structure
-------------------------------------------------------------------------------
data NilM (m :: * -> *) = NilM
  deriving Show

data (a :> b) m = m a :> b m
  deriving Show

data (u ::> b) (m :: * -> *) = u m ::> b m
  deriving Show

infixr 6 :>
infixr 5 ::>

--getField (a :> b) = a
--dropField (a :> b) = b

instance FunctorS NilM where
    mapS f NilM = NilM

instance ApplicativeS NilM NilM where
    appS NilM NilM = NilM

instance TraversableS NilM where
    seqS NilM = pure NilM

instance ParseS NilM NilM where
  parseS NilM = pure NilM


instance FunctorS b => FunctorS (a :> b) where
    mapS f (ma :> bm) = unFunc(unForall f) ma :> mapS f bm

instance ApplicativeS b1 b2 => ApplicativeS (a :> b1) (a :> b2) where
    appS (ma1 :> bm1) (ma2 :> bm2) = (unFunc ma1 ma2) :> appS bm1 bm2

instance TraversableS b => TraversableS (a :> b) where
    seqS (ma :> bm) = (:>) <$> unComp ma <*> seqS bm

instance ParseS b1 b2 => ParseS (a :> b1) (a :> b2) where
  parseS (ma :> bm) = (:>) <$> fmap pure ma <*> parseS bm


instance (FunctorS u, FunctorS b) => FunctorS (u ::> b) where
    mapS f (um ::> bm) = mapS f um ::> mapS f bm

instance (ApplicativeS u1 u2, ApplicativeS b1 b2) => ApplicativeS (u1 ::> b1) (u2 ::> b2) where
    appS (um1 ::> bm1) (um2 ::> bm2) = appS um1 um2 ::> appS bm1 bm2

instance (TraversableS u, TraversableS b) => TraversableS (u ::> b) where
    seqS (um ::> bm) = (::>) <$> seqS um <*> seqS bm

instance (ParseS a1 a2, ParseS b1 b2) => ParseS (a1 ::> b1) (a2 ::> b2) where
  parseS (um ::> bm) = (::>) <$> parseS um <*> parseS bm


--instance (ParseS t1 t1, ParseS t2 t2) => ParseS (Product t1 t2) (Sum t1 t2) where
--  parseS (Product t1 t2) = (LeftS <$> parseS t1) <|> (RightS <$> parseS t2)
