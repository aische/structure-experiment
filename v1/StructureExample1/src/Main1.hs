{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}


module Main1 where


import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor.Identity
import           Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map


import           Control.Structure


import           Decoder
import           Dialog


data Person f = Person
  { firstname :: f String
  , lastname :: f String
  , age :: f Int
  }


deriving instance Show (Person Identity)


instance Structure Person where
  mapS f (Person firstname lastname age) = Person
    (unFunc (unForall f) firstname)
    (unFunc (unForall f) lastname)
    (unFunc (unForall f) age)

  appS (Person ffirstname flastname fage) (Person firstname lastname age) = Person
    (unFunc ffirstname firstname)
    (unFunc flastname lastname)
    (unFunc fage age)

  seqS (Person firstname lastname age) = Person
    <$> unComp firstname
    <*> unComp lastname
    <*> unComp age


-------------------------------------------------------------------------------
-- constant
person1 :: Person Identity
person1 = Person
  { firstname = Identity "Max"
  , lastname = Identity "Mustermann"
  , age = Identity 10
  }


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
personDecoder = Person
  { firstname = stringDecoder "firstname"
  , lastname = stringDecoder "lastname"
  , age = intDecoder "age"
  }


-------------------------------------------------------------------------------
-- command line dialog
personDialog :: Person IO
personDialog = Person
  { firstname = dialogPlain "firstname?"
  , lastname = dialogPlain "lastname?"
  , age = dialogRead "age?"
  }


-------------------------------------------------------------------------------
-- fold
personFold :: Monad m => Person (Func Identity (StateT a m))
personFold = Person
  { firstname = foldFIgnore
  , lastname = foldFIgnore
  , age = foldFIgnore
  }


-- fold for extracting fullname
personF_getFullName :: Person (Func Identity (State FullName))
personF_getFullName = personFold
  { firstname = foldFWrite $ \s name -> s { fullNameFirst = name }
  , lastname = foldFWrite $ \s name -> s { fullNameLast = name }
  }


data FullName = FullName { fullNameFirst :: String, fullNameLast :: String }
  deriving Show


getFullName :: Person Identity -> FullName
getFullName p = execState (foldS personF_getFullName p) (FullName "" "")


-- fold for extracting age
personF_getAge :: Person (Func Identity (State Int))
personF_getAge = personFold
  { age = foldFWrite $ \s age -> age
  }


getAge :: Person Identity -> Int
getAge p = execState (foldS personF_getAge p) 0


-- fold for reversing first- and lastname
personF_reverseName :: Person (Func Identity (State ()))
personF_reverseName = personFold
  { firstname = foldFRead $ \s name -> List.reverse name
  , lastname = foldFRead $ \s name -> List.reverse name
  }


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

