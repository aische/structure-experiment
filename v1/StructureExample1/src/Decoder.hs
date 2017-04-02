module Decoder where


import           Data.Functor.Identity
import           Control.Monad.Reader
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (listToMaybe)


import           Control.Structure


type PseudoJson = Map String String


type Decoder = ReaderT PseudoJson Maybe


stringDecoder :: String -> Decoder String
stringDecoder name = ReaderT $ \m ->
  Map.lookup name m


intDecoder :: String -> Decoder Int
intDecoder name = ReaderT $ \m ->
  Map.lookup name m >>= maybeRead


boolDecoder :: String -> Decoder Bool
boolDecoder name = ReaderT $ \m ->
  Map.lookup name m >>= maybeRead


maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads


decodeS :: Structure t => t Decoder -> PseudoJson -> Maybe (t Identity)
decodeS t = toIdentity . runReader (readS t)

