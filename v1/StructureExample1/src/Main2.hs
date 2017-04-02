{-# LANGUAGE TypeOperators #-}


module Main2 where


import           Control.Applicative
import           Control.Monad.State
import           Data.Functor.Identity
import           Data.List as List
import           Data.Proxy


import           Control.Structure
import           Data.Structure.List
import           Data.Structure.Fields


type T1 = Int :> String :> NilM


type T2 = Bool :> T1 ::> String :> NilM


fieldsT1 :: T1 (Field m T1)
fieldsT1 = fields


ageFieldT1 :> fnameT1 :> NilM = fieldsT1


fieldsT2 :: T2 (Field m T2)
fieldsT2 = fields


active :> (ageField :> fname :> NilM) ::> lname :> NilM = fieldsT2


t2 :: T2 Identity
t2 = pure True :> (pure 1 :> pure "Max" :> NilM) ::> pure "Mustermann" :> NilM


t2Fold :: Monad m => T2 (FoldF m String)
t2Fold = (foldFWrite $ \s active -> s ++ show active )
      :> (  (foldFWrite $ \s age -> s ++ show age )
         :> (foldFWrite $ \s fname -> s ++ fname )
         :> NilM
         )
      ::> (foldFWrite $ \s lname -> s ++ lname )
      :> NilM


main = do
  print t2
  let t2' = setField ageField 99 t2
  print t2'
  let (_, s) = runState (foldS t2Fold t2') ""
  print s











