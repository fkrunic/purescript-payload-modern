-- | This module contains various helpers for returning server
-- | responses.
module Payload.Server.Response
  ( status
  , setStatus
  , updateStatus
  , setBody
  , updateBody
  , setHeaders
  , updateHeaders
  , class ToSpecResponse
  , toSpecResponse
  , class EncodeResponse
  , encodeResponse
  , ok
  , created
  , badRequest
  , notFound
  , internalError
  ) where

import Prelude
import Control.Monad.Except.Trans (except, throwError)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Node.Stream as Stream
import Payload.Headers (Headers)
import Payload.Headers as Headers
import Payload.ResponseTypes (Empty, Failure(..), HttpStatus, Json(..), RawResponse, Response(..), ResponseBody(..), Result)
import Payload.ContentType as ContentType
import Payload.Server.Status as Status
import Payload.TypeErrors (type (<>), type (|>))
import Prim.TypeError (class Fail, Quote, Text)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

status :: forall a. HttpStatus -> a -> Response a
status s body = Response { status: s, headers: Headers.empty, body }

setStatus :: forall a. HttpStatus -> Response a -> Response a
setStatus s = over Response (_ { status = s })

updateStatus :: forall a. (HttpStatus -> HttpStatus) -> Response a -> Response a
updateStatus f (Response res) = Response (res { status = f res.status })

setBody :: forall a b. b -> Response a -> Response b
setBody body = over Response (_ { body = body })

updateBody :: forall a b. (a -> b) -> Response a -> Response b
updateBody f (Response res) = Response (res { body = f res.body })

setHeaders :: forall a. Headers -> Response a -> Response a
setHeaders headers = over Response (_ { headers = headers })

updateHeaders :: forall a. (Headers -> Headers) -> Response a -> Response a
updateHeaders f (Response res) = Response (res { headers = f res.headers })

-- | This type class is for converting types which are compatible with
-- | the spec into the spec type.
-- | If the spec says one type is returned from an endpoint, a handler
-- | can either return that type directly or return another type from
-- | which that type can be produced (e.g. a full response with different
-- | headers or a different status code).
class ToSpecResponse (docRoute :: Symbol) a b where
  toSpecResponse :: Proxy docRoute -> a -> Result (Response b)

instance toSpecResponseEitherFailureVal :: EncodeResponse a => ToSpecResponse docRoute (Either Failure a) a where
  toSpecResponse _ (Left err) = throwError err
  toSpecResponse _ (Right res) = pure (ok res)
else instance toSpecResponseEitherFailureResponse :: EncodeResponse a => ToSpecResponse docRoute (Either Failure (Response a)) a where
  toSpecResponse _ (Left err) = throwError err
  toSpecResponse _ (Right res) = pure res
else instance toSpecResponseEitherResponseVal :: EncodeResponse err => ToSpecResponse docRoute (Either (Response err) a) a where
  toSpecResponse _ (Left res) = do
    raw <- except $ encodeResponse res
    throwError (Error raw)
  toSpecResponse _ (Right res) = pure (ok res)
else instance toSpecResponseEitherResponseResponse :: EncodeResponse err => ToSpecResponse docRoute (Either (Response err) (Response a)) a where
  toSpecResponse _ (Left res) = do
    raw <- except $ encodeResponse res
    throwError (Error raw)
  toSpecResponse _ (Right res) = pure res
else instance toSpecResponseEitherValVal :: (EncodeResponse a, EncodeResponse err) => ToSpecResponse docRoute (Either err a) a where
  toSpecResponse _ (Left res) = do
    raw <- except $ encodeResponse (internalError res)
    throwError (Error raw)
  toSpecResponse _ (Right res) = pure (ok res)
else instance toSpecResponseEitherValResponse :: (EncodeResponse a, EncodeResponse err) => ToSpecResponse docRoute (Either err (Response a)) a where
  toSpecResponse _ (Left res) = do
    raw <- except $ encodeResponse (internalError res)
    throwError (Error raw)
  toSpecResponse _ (Right res) = pure res
else instance toSpecResponseResponse ::
  EncodeResponse a =>
  ToSpecResponse docRoute (Response a) a where
  toSpecResponse _ res = pure res
else instance toSpecResponseIdentity ::
  EncodeResponse a =>
  ToSpecResponse docRoute a a where
  toSpecResponse _ res = pure (ok res)
else instance toSpecResponseFail ::
  ( Fail ( Text "Could not match or convert handler response type to spec response type."
      |>
      Text ""
      |>
      Text "           Route: "
      <>
      Text docRoute
      |>
      Text "Handler response: "
      <>
      Quote a
      |>
      Text "   Spec response: "
      <>
      Quote b
      |>
      Text ""
      |>
      Text "Specifically, no type class instance was found for"
      |>
      Text ""
      |>
      Text "ToSpecResponse docRoute"
      |>
      Text "               "
      <>
      Quote a
      |>
      Text "               "
      <>
      Quote b
      |>
      Text ""
    )
  ) =>
  ToSpecResponse docRoute a b where
  toSpecResponse res = unsafeCoerce res

-- | Any types that can appear in a server response body and show up in the API
-- | spec under the "body" field must implement EncodeResponse. This is also
-- | a good place to add a Content-Type header for the encoded response.
class EncodeResponse r where
  encodeResponse :: Response r -> Either Failure RawResponse

instance encodeResponseResponseBody :: EncodeResponse ResponseBody where
  encodeResponse = Right

instance encodeResponseRecord :: EncodeJson (Record r) => EncodeResponse (Record r) where
  encodeResponse (Response r) = encodeResponse $ Response $ r { body = Json r.body }

instance encodeResponseArray :: EncodeJson (Array r) => EncodeResponse (Array r) where
  encodeResponse (Response r) = encodeResponse $ Response $ r { body = Json r.body }

instance encodeResponseJson :: EncodeJson r => EncodeResponse (Json r) where
  encodeResponse (Response { body: Json json, status, headers }) =
    Right
      $ Response
          { status
          , headers: Headers.setIfNotDefined "content-type" ContentType.json headers
          , body: StringBody (stringify $ encodeJson json)
          }

instance encodeResponseString :: EncodeResponse String where
  encodeResponse (Response r) =
    Right
      $ Response
          { status: r.status
          , headers: Headers.setIfNotDefined "content-type" ContentType.plain r.headers
          , body: StringBody r.body
          }

instance encodeResponseStream ::
  TypeEquals (Stream.Stream r) (Stream.Stream ( read :: Stream.Read | r' )) =>
  EncodeResponse (Stream.Stream r) where
  encodeResponse (Response { status, headers, body }) =
    Right
      $ Response
          { status
          , headers: Headers.setIfNotDefined "content-type" ContentType.plain headers
          , body: StreamBody (unsafeCoerce body)
          }

instance encodeResponseMaybe :: EncodeResponse a => EncodeResponse (Maybe a) where
  encodeResponse (Response { body: Nothing }) =
    Right
      $ Response
          { status: Status.notFound
          , headers: Headers.empty
          , body: EmptyBody
          }
  encodeResponse (Response { body: Just body, status, headers }) =
    encodeResponse $ Response { status, headers, body }

instance encodeResponseEmpty :: EncodeResponse Empty where
  encodeResponse (Response { status, headers }) =
    Right $ Response { status, headers, body: EmptyBody }

-- -- | Status code: 200
ok :: forall a. a -> Response a
ok = status Status.ok

-- | Status code: 201
created :: forall a. a -> Response a
created = status Status.created

-- | Status code: 400
badRequest :: forall a. a -> Response a
badRequest = status Status.badRequest

-- | Status code: 404
notFound :: forall a. a -> Response a
notFound = status Status.notFound

-- | Status code: 500
internalError :: forall a. a -> Response a
internalError = status Status.internalError
