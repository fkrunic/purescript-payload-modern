module Payload.Client.QueryParams where

import Prelude
import Data.Either (Either(..), note)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Traversable (sequence)
import Foreign.Object (Object)
import Foreign.Object as Object
import JSURI (encodeURIComponent)

data EncodingError 
  = EncodingError String
  | EmptyEncoding

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

class EncodeQueryParam a where
  encodeQueryParam :: a -> Either EncodingError (Maybe String)

class EncodeQueryParamMulti a where
  encodeQueryParamMulti :: a -> Maybe String  

instance encodeQueryParamInt :: EncodeQueryParam Int where
  encodeQueryParam val = Right $ Just (show val)

instance encodeQueryParamString :: EncodeQueryParam String where
  encodeQueryParam s = note (EncodingError s) (encodeURIComponent s) >>= (Just >>> Right)

instance encodeQueryParamBoolean :: EncodeQueryParam Boolean where
  encodeQueryParam true = Right $ Just "true"
  encodeQueryParam false = Right $ Just "false"

instance encodeQueryParamMaybe :: EncodeQueryParam a => EncodeQueryParam (Maybe a) where
  encodeQueryParam (Just val) = encodeQueryParam val
  encodeQueryParam Nothing = Right Nothing

instance encodeQueryParamMultiObjectArrayString :: EncodeQueryParamMulti (Object (Array String)) where
  encodeQueryParamMulti o = do 
    encoded <- sequence $ map encodeArray $ Object.toUnfoldable o
    Just $ joinParams encoded

derive instance genericEncodingError :: Generic EncodingError _ 

instance eqEncodingError :: Eq EncodingError where 
  eq = genericEq

instance showEncodingError :: Show EncodingError where 
  show = genericShow

