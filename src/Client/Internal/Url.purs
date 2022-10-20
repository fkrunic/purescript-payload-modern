module Payload.Client.Internal.Url where

import Prelude
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String as String
import Payload.Client.EncodeParam (class EncodeParam, encodeParam)
import Payload.Client.QueryParams (EncodingError(..))
import Payload.Internal.UrlParsing (class ParseUrl, UrlListProxy(..), Key, Lit, Multi, UrlCons, UrlNil, UrlList)
import Prim.Row as Row
import Record as Record
import Type.Prelude (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

class EncodeUrl (urlStr :: Symbol) (params :: Row Type) | urlStr -> params where
  encodeUrl :: Proxy urlStr -> Record params -> Either EncodingError (Maybe String)

instance encodeUrlRecord :: (ParseUrl urlStr urlParts, WriteUrl urlParts params) => EncodeUrl urlStr params where
  encodeUrl _ params = writeUrl (UrlListProxy :: _ urlParts) params

class WriteUrl (urlParts :: UrlList) (params :: Row Type) where
  writeUrl :: UrlListProxy urlParts -> Record params -> Either EncodingError (Maybe String)

instance writeUrlUrlNil :: WriteUrl UrlNil params where
  writeUrl _ _ = Right $ Just ""

instance writeUrlConsKey ::
  ( IsSymbol key
  , Row.Cons key valType restOfParams params
  , Row.Lacks key restOfParams
  , EncodeParam valType
  , WriteUrl rest restOfParams
  ) =>
  WriteUrl (UrlCons (Key key) rest) params where
  writeUrl _ params = do 
    encodedParam <- encodeParam (Record.get (Proxy :: Proxy key) params)
    let 
      restOfParams = Record.delete (Proxy :: _ key) params
    restOfUrl <- writeUrl (UrlListProxy :: _ rest) restOfParams
    case restOfUrl of 
      Just content -> Right $ Just $ "/" <> encodedParam <> content
      Nothing -> Left EmptyEncoding
    

instance writeUrlConsLit ::
  ( IsSymbol lit
  , WriteUrl rest params
  ) =>
  WriteUrl (UrlCons (Lit lit) rest) params where
  writeUrl _ params = do 
    restOfUrl <- writeUrl (UrlListProxy :: _ rest) params
    let 
      litStr = reflectSymbol (Proxy :: Proxy lit)
    case restOfUrl of 
      Just content -> Right $ Just $ "/" <> litStr <> content
      Nothing -> Left EmptyEncoding
    

instance writeUrlConsMulti ::
  ( IsSymbol multiKey
  , Row.Cons multiKey (List String) () params
  ) =>
  WriteUrl (UrlCons (Multi multiKey) UrlNil) params where
  writeUrl _ params = Right $ Just $ "/" <> multiStr
    where
    multiList = Record.get (Proxy :: _ multiKey) params
    multiStr = String.joinWith "/" (Array.fromFoldable multiList)
