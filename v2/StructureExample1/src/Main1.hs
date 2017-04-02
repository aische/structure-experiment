{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}


module Main1 where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor.Identity

import           Control.Structure
import           Data.Structure.List
import           Data.Structure.Fields
-------------------------------------------------------------------------------
data Person f = Person
  { firstname :: f String
  , lastname :: f String
  , age :: f Int
  }

deriving instance Show (Person Identity)

instance FunctorS Person where
  mapS f (Person firstname lastname age) = Person
    (unFunc(unForall f) firstname)
    (unFunc(unForall f) lastname)
    (unFunc(unForall f) age)

instance ApplicativeS Person Person where
  appS (Person ffirstname flastname fage) (Person firstname lastname age) = Person
    (unFunc ffirstname firstname)
    (unFunc flastname lastname)
    (unFunc fage age)

instance TraversableS Person where
  seqS (Person firstname lastname age) = Person
    <$> unComp firstname
    <*> unComp lastname
    <*> unComp age
-------------------------------------------------------------------------------
data Robot f = Robot
  { robotname :: f String
  , version :: f Int
  }

deriving instance Show (Robot Identity)

instance FunctorS Robot where
  mapS f (Robot robotname version) = Robot
    (unFunc(unForall f) robotname)
    (unFunc(unForall f) version)

instance ApplicativeS Robot Robot where
  appS (Robot frobotname fversion) (Robot robotname version) = Robot
    (unFunc frobotname robotname)
    (unFunc fversion version)

instance TraversableS Robot where
  seqS (Robot robotname version) = Robot
    <$> unComp robotname
    <*> unComp version
---------------------------------------------------------------------------------
data Score e f = Score
  { player :: e Person Robot f
  , points :: f Int
  }

deriving instance Show (Score Product Identity)
deriving instance Show (Score Sum Identity)

instance FunctorS (Score Product) where
  mapS f (Score player points) = Score
    (mapS f player)
    (unFunc(unForall f) points)

instance FunctorS (Score Sum) where
  mapS f (Score player points) = Score
    (mapS f player)
    (unFunc(unForall f) points)

instance ApplicativeS (Score Product) (Score Product) where
  appS (Score fplayer fpoints) (Score player points) = Score
    (appS fplayer player)
    (unFunc fpoints points)

instance ApplicativeS (Score Product) (Score Sum) where
  appS (Score fplayer fpoints) (Score player points) = Score
    (appS fplayer player)
    (unFunc fpoints points)

instance TraversableS (Score Product) where
  seqS (Score player points) = Score
    <$> seqS player
    <*> unComp points

instance TraversableS (Score Sum) where
  seqS (Score player points) = Score
    <$> seqS player
    <*> unComp points
---------------------------------------------------------------------------------

po1 :: Score Product (Func Identity Identity)
po1 = Score
  { player = Product
      (Person
        (Func $ \(Identity x) -> Identity (x ++ x))
        (Func $ \(Identity x) -> Identity (x ++ x))
        (Func $ \(Identity x) -> Identity (x + x)))

      (Robot
        (Func $ \(Identity x) -> Identity (x ++ x))
        (Func $ \(Identity x) -> Identity (x + x)))
  , points = (Func $ \(Identity x) -> Identity (x * 10))
  }


po2 :: Score Sum Identity
po2 = Score
  { player = LeftS (Person (pure "max") (pure "muster") (pure 10))
  , points = pure 100
  }

xxxxx = appS (composeS po1 po1) po2

main = print xxxxx

--t Product Maybe






