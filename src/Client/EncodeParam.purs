module Payload.Client.EncodeParam
  ( class EncodeParam
  , encodeParam
  ) where

import Prelude
import Data.Either (Either(..), note)
import JSURI (encodeURIComponent)
import Payload.Client.QueryParams (EncodingError(..))

class EncodeParam a where
  encodeParam :: a -> Either EncodingError String

instance encodeParamInt :: EncodeParam Int where
  encodeParam = show >>> Right

instance encodeParamString :: EncodeParam String where
  encodeParam p = note (EncodingError p) $ encodeURIComponent p
