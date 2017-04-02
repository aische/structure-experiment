{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}


module Main2 where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor.Identity

import           Control.Structure
import           Data.Structure.List
import           Data.Structure.Fields
-------------------------------------------------------------------------------
type TT1 e = e (String :> NilM) (Int :> NilM) ::> NilM

tt1_f :: TT1 Product (Func Identity Identity)
tt1_f =
  Product
    ( (Func $ \(Identity x) -> Identity (x ++ x)) :> NilM)
    ( (Func $ \(Identity x) -> Identity (x * 2)) :> NilM)
  ::> NilM

tt1_a1 :: TT1 Product Identity
tt1_a1 =
  Product
    ( pure "hello" :> NilM)
    ( pure 99 :> NilM)
  ::> NilM


tt1_a2 :: TT1 Sum Identity
tt1_a2 =
  LeftS
    ( pure "hello" :> NilM)
  ::> NilM


tt1_result1 = appS tt1_f tt1_a1

tt1_result2 = appS tt1_f tt1_a2

tt1_result3 = appS (composeS tt1_f tt1_f) tt1_a1

tt1_result4 = appS (composeS tt1_f tt1_f) tt1_a2


parseNumber :: String -> Maybe Int
parseNumber s =
  case reads s of
    (n, ""):_ -> Just n
    _         -> Nothing

parseString :: String -> Maybe String
parseString s =
  case reads s of
    (n, ""):_ -> Just n
    _         -> Nothing


tt1_parser :: TT1 Product (ReaderT String Maybe)
tt1_parser =
  Product
    ( (ReaderT $ parseString ) :> NilM)
    ( (ReaderT $ parseNumber) :> NilM)
  ::> NilM


tt1_result5 :: Maybe (TT1 Sum Identity)
tt1_result5 = runReaderT (parseS tt1_parser) "123"

tt1_result6 :: Maybe (TT1 Sum Identity)
tt1_result6 = runReaderT (parseS tt1_parser) "\"hello\""

tt1_result7 :: Maybe (TT1 Sum Identity)
tt1_result7 = runReaderT (parseS tt1_parser) ""

main = do
  print tt1_result5
  print tt1_result6

