module Payload.Server.DecodeBody
  ( class DecodeBody
  , decodeBody
  ) where

import Prelude
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..))

class DecodeBody body where
  decodeBody :: String -> Either String body

instance decodeBodyRecord :: DecodeJson (Record r) => DecodeBody (Record r) where
  decodeBody s = do 
    parsed <- lmap show $ parseJson s 
    lmap show $ decodeJson parsed

instance decodeBodyArray :: DecodeJson (Array r) => DecodeBody (Array r) where
  decodeBody s = do 
    parsed <- lmap show $ parseJson s 
    lmap show $ decodeJson parsed

instance decodeBodyString :: DecodeBody String where
  decodeBody = pure

instance decodeBodyMaybe :: DecodeBody a => DecodeBody (Maybe a) where
  decodeBody "" = pure Nothing
  decodeBody str = Just <$> decodeBody str
