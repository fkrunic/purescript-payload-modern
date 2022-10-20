module Payload.Client.Internal.Query where

import Payload.Client.QueryParams

import Data.Array as Array
import Data.List (List(..), (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Payload.Internal.QueryParsing (Key, Multi, class ParseQuery, QueryCons, QueryListProxy(..), QueryNil, QueryList)
import Prelude (bind, ($), (<>))
import Prim.Row as Row
import Record as Record
import Type.Prelude (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

class EncodeQuery (urlStr :: Symbol) (query :: Row Type) | urlStr -> query where
  encodeQuery ::
    Proxy urlStr ->
    Record query ->
    Either EncodingError String

class EncodeQueryList (queryParts :: QueryList) (query :: Row Type) where
  encodeQueryList ::
    QueryListProxy queryParts ->
    Record query ->
    Either EncodingError (List String)    

instance encodeQuerySymbol :: ( ParseQuery urlStr queryParts, EncodeQueryList queryParts query) => EncodeQuery urlStr query where
  encodeQuery _ query = do
    encoded <- encodeQueryList (QueryListProxy :: _ queryParts) query :: Either EncodingError (List String)
    case encoded of
      Nil -> Right ""
      e -> Right $ "?" <> String.joinWith "&" (Array.fromFoldable e)

instance encodeQueryListNil :: EncodeQueryList QueryNil query where
  encodeQueryList _ _ = Right Nil

instance encodeQueryListConsKey ::
  ( IsSymbol queryKey
  , IsSymbol ourKey
  , Row.Cons ourKey valType queryRest query
  , Row.Lacks ourKey queryRest
  , EncodeQueryParam valType
  , EncodeQueryList rest queryRest
  ) =>
  EncodeQueryList
    (QueryCons (Key queryKey ourKey) rest)
    query where
  encodeQueryList _ query = do 
    encodedValue :: Maybe String <- encodeQueryParam val
    case encodedValue of
      Just encoded -> do 
        r <- rest 
        Right $ (label <> "=" <> encoded) : r
      Nothing -> rest
    where
    label = reflectSymbol (Proxy :: Proxy queryKey)
    val = Record.get (Proxy :: Proxy ourKey) query
    queryRest = Record.delete (Proxy :: Proxy ourKey) query
    rest = encodeQueryList (QueryListProxy :: _ rest) queryRest

instance encodeQueryListConsMulti ::
  ( IsSymbol ourKey
  , Row.Cons ourKey valType () query
  , EncodeQueryParamMulti valType
  ) =>
  EncodeQueryList (QueryCons (Multi ourKey) QueryNil) query where
  encodeQueryList _ query = case encodeQueryParamMulti queryObj of
    Just encoded -> Right $ encoded : Nil
    Nothing -> Right Nil
    where
    queryObj = Record.get (Proxy :: _ ourKey) query
