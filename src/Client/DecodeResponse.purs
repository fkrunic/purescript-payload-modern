module Payload.Client.DecodeResponse where

import Prelude
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Foreign (MultipleErrors)
import Node.Stream as Stream
import Payload.ResponseTypes (ResponseBody(..))
import Payload.TypeErrors (type (<>), type (|>))
import Prim.TypeError (class Warn, Quote, Text)
import Type.Equality (class TypeEquals)
import Unsafe.Coerce (unsafeCoerce)

data DecodeResponseError
  = InternalDecodeError { message :: String }
  | UnhandledResponseError { message :: String }
  | JsonDecodingError JsonDecodeError

unexpectedError :: forall a. String -> ResponseBody -> Either DecodeResponseError a
unexpectedError expected body = Left (InternalDecodeError { message })
  where
  received = case body of
    StringBody s -> "(StringBody '" <> s <> "')"
    StreamBody _ -> "StreamBody"
    EmptyBody -> "EmptyBody"
  message =
    "Invalid response type, expected '"
      <> expected
      <> "' but received '"
      <> received
      <> "'."
      <> "This is probably a bug in the library."

unhandled :: String -> DecodeResponseError
unhandled message = UnhandledResponseError { message }

class DecodeResponse r where
  decodeResponse :: ResponseBody -> Either DecodeResponseError r

instance decodeResponseString :: DecodeResponse String where
  decodeResponse (StringBody s) = Right s
  decodeResponse b = unexpectedError "StringBody" b

else instance decodeResponseStream :: 
  TypeEquals (Stream.Stream r) (Stream.Stream ( read :: Stream.Read | r' )) =>
  DecodeResponse (Stream.Stream r) where

  decodeResponse (StreamBody s) = Right (unsafeCoerce s)
  decodeResponse b = unexpectedError "StreamBody" b

else instance decodeResponseRecord :: DecodeJson (Record r) => DecodeResponse (Record r) where
  decodeResponse (StringBody s) = do 
    parsed <- lmap JsonDecodingError $ parseJson s
    lmap JsonDecodingError $ decodeJson parsed
    
  decodeResponse b = unexpectedError "StringBody" b

else instance decodeResponseArray :: DecodeJson (Array r) => DecodeResponse (Array r) where
  decodeResponse (StringBody s) = do 
    parsed <- lmap JsonDecodingError $ parseJson s
    lmap JsonDecodingError $ decodeJson parsed

  decodeResponse b = unexpectedError "StringBody" b
-- | Adding a default instance allows the client to be incomplete:
-- | not all responses are supported.

else instance decodeResponseDefault ::
  Warn ( Text "API client cannot query all of endpoints in API spec:"
      |>
      Text ""
      |>
      Text "No type class instance was found for"
      |>
      Text ""
      |>
      Text "DecodeResponse "
      <>
      Quote a
      |>
      Text ""
    ) =>
  DecodeResponse a where
  decodeResponse _ = Left (unhandled "Could not decode response - no DecodeResponse instance")

instance showDecodeResponseError :: Show DecodeResponseError where
  show (InternalDecodeError { message }) = "InternalDecodeError '" <> message <> "'"
  show (UnhandledResponseError { message }) = "UnhandledResponseError '" <> message <> "'"
  show (JsonDecodingError err) = "JsonDecodeError: " <> show err <> "'"

instance eqDecodeResponseError :: Eq DecodeResponseError where
  eq (InternalDecodeError a) (InternalDecodeError b) = a == b
  eq (UnhandledResponseError a) (UnhandledResponseError b) = a == b
  eq (JsonDecodingError a) (JsonDecodingError b) = a == b
  eq _ _ = false