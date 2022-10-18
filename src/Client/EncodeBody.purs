module Payload.Client.EncodeBody where

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Maybe (Maybe(..))
import Payload.ContentType (class HasContentType)
import Prelude ((>>>))

class
  (HasContentType body) <= EncodeBody body where
  encodeBody :: body -> String

instance encodeBodyString :: EncodeBody String where
  encodeBody b = b

instance encodeBodyRecord :: EncodeJson (Record r) => EncodeBody (Record r) where
  encodeBody = encodeJson >>> stringify

instance encodeBodyArray :: EncodeJson (Array r) => EncodeBody (Array r) where
  encodeBody = encodeJson >>> stringify

instance encodeBodyMaybe :: EncodeBody a => EncodeBody (Maybe a) where
  encodeBody Nothing = ""
  encodeBody (Just body) = encodeBody body
