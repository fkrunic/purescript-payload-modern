module Payload.Client.EncodeParam
  ( class EncodeParam
  , encodeParam
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import JSURI (encodeURIComponent)

class EncodeParam a where
  encodeParam :: a -> Maybe String

instance encodeParamInt :: EncodeParam Int where
  encodeParam = show >>> Just

instance encodeParamString :: EncodeParam String where
  encodeParam = encodeURIComponent
