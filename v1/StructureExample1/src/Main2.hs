{-# LANGUAGE TypeOperators #-}


module Main2 where


import           Control.Applicative
import           Control.Monad.State
import           Data.Functor.Identity
import           Data.List as List
import           Data.Proxy
import           Data.Map (Map)
import qualified Data.Map as Map


import           Control.Structure
import           Data.Structure.List
import           Data.Structure.Fields


import           Decoder
import           Dialog


type Person = String :> String :> Int :> NilM


fieldsPerson :: Person (Field m Person)
fieldsPerson = fields


firstname :> lastname :> age :> NilM = fieldsPerson


-------------------------------------------------------------------------------
-- constant
person1 :: Person Identity
person1 = Identity "Max" :> Identity "Mustermann" :> Identity 10 :> NilM


-------------------------------------------------------------------------------
-- json decoder
person2 :: Maybe (Person Identity)
person2 = decodeS personDecoder personPseudoJson1


personPseudoJson1 :: PseudoJson
personPseudoJson1 = Map.fromList
  [ ("firstname", "John")
  , ("lastname", "Doe")
  , ("age", "99")
  ]


personDecoder :: Person Decoder
personDecoder = stringDecoder "firstname" :> stringDecoder "lastname" :> intDecoder "age" :> NilM


-------------------------------------------------------------------------------
-- command line dialog
personDialog :: Person IO
personDialog = dialogPlain "firstname?" :> dialogPlain "lastname?" :> dialogRead "age?" :> NilM


-------------------------------------------------------------------------------
-- fold
personFold :: Monad m => Person (Func Identity (StateT a m))
personFold = foldFIgnore :> foldFIgnore :> foldFIgnore :> NilM


-- fold for extracting fullname
personF_getFullName :: Person (Func Identity (State FullName))
personF_getFullName =
  setField firstname (foldFWrite $ \s name -> s { fullNameFirst = name }) $
  setField lastname (foldFWrite $ \s name -> s { fullNameLast = name }) $
  personFold


data FullName = FullName { fullNameFirst :: String, fullNameLast :: String }
  deriving Show


getFullName :: Person Identity -> FullName
getFullName p = execState (foldS personF_getFullName p) (FullName "" "")


-- fold for extracting age
personF_getAge :: Person (Func Identity (State Int))
personF_getAge =
  setField age (foldFWrite $ \s age -> age) $
  personFold


getAge :: Person Identity -> Int
getAge p = execState (foldS personF_getAge p) 0


-- fold for reversing first- and lastname
personF_reverseName :: Person (Func Identity (State ()))
personF_reverseName =
  setField firstname (foldFRead $ \s name -> List.reverse name) $
  setField lastname (foldFRead $ \s name -> List.reverse name) $
  personFold


reverseName :: Person Identity -> Person Identity
reverseName p = evalState (foldS personF_reverseName p) ()


-------------------------------------------------------------------------------
main1 = do
  print $ getFullName person1
  print $ getAge person1
  print $ reverseName person1


main2 = do
  p <- runDialog personDialog
  print $ getFullName p
  print $ getAge p
  print $ reverseName p

