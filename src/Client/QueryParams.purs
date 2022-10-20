module Payload.Client.QueryParams
  ( class EncodeQueryParam
  , encodeQueryParam
  , class EncodeQueryParamMulti
  , encodeQueryParamMulti
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Traversable (sequence)
import Foreign.Object (Object)
import Foreign.Object as Object
import JSURI (encodeURIComponent)

class EncodeQueryParam a where
  encodeQueryParam :: a -> Maybe String

instance encodeQueryParamInt :: EncodeQueryParam Int where
  encodeQueryParam val = Just (show val)

instance encodeQueryParamString :: EncodeQueryParam String where
  encodeQueryParam = encodeURIComponent

instance encodeQueryParamBoolean :: EncodeQueryParam Boolean where
  encodeQueryParam true = Just "true"
  encodeQueryParam false = Just "false"

instance encodeQueryParamMaybe :: EncodeQueryParam a => EncodeQueryParam (Maybe a) where
  encodeQueryParam (Just val) = encodeQueryParam val
  encodeQueryParam Nothing = Nothing

joinParams :: Array String -> String
joinParams = String.joinWith "&"

encodeArray :: Tuple String (Array String) -> Maybe String
encodeArray (Tuple k vals) = do 
  encoded <- sequence $ map (encodeVal k) vals
  Just $ joinParams encoded

encodeVal :: String -> String -> Maybe String
encodeVal key val = do 
  encodedKey <- encodeURIComponent key
  encodedValue <- encodeURIComponent val
  Just $ encodedKey <> "=" <> encodedValue 

class EncodeQueryParamMulti a where
  encodeQueryParamMulti :: a -> Maybe String

instance encodeQueryParamMultiObjectArrayString :: EncodeQueryParamMulti (Object (Array String)) where
  encodeQueryParamMulti o = do 
    encoded <- sequence $ map encodeArray $ Object.toUnfoldable o
    Just $ joinParams encoded
