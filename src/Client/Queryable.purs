module Payload.Client.Queryable where

import Prelude
import Affjax.Node as AX
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseHeader (ResponseHeader(..))
import Affjax.StatusCode (StatusCode(..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.HTTP.Method (CustomMethod, Method(..), unCustomMethod)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.MediaType (MediaType(..))
import Data.String (Pattern(..), joinWith, stripSuffix) as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Payload.Client.DecodeResponse (class DecodeResponse, DecodeResponseError, decodeResponse)
import Payload.Client.EncodeBody (class EncodeBody, encodeBody)
import Payload.Client.Internal.Query (class EncodeQuery, encodeQuery)
import Payload.Client.Internal.Url as PayloadUrl
import Payload.Client.Options (LogLevel(..), Options, RequestOptions)
import Payload.Client.Response (ClientError(..), ClientResponse)
import Payload.ContentType (class HasContentType, getContentType)
import Payload.Headers (Headers)
import Payload.Headers as Headers
import Payload.Internal.Route (DefaultRouteSpec, Undefined)
import Payload.ResponseTypes (Response(..), ResponseBody(..), HttpStatus)
import Payload.Spec (Route)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Prim.Symbol as Symbol
import Record as Record
import Type.Equality (class TypeEquals, to)
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)

type ClientFnWithOptions payload body
  = RequestOptions -> ClientFn payload body

type ClientFn payload body
  = payload -> Aff (ClientResponse body)

class Queryable route (basePath :: Symbol) (baseParams :: Row Type) payload res | route baseParams basePath -> payload
, route -> res where
  request ::
    route ->
    Proxy basePath ->
    Proxy (Record baseParams) ->
    Options ->
    ClientFnWithOptions payload res

type Request
  = { method :: Method
    , url :: String
    , body :: Maybe RequestBody.RequestBody
    , headers :: Array RequestHeader
    , opts :: Options
    , reqOpts :: RequestOptions
    }    

makeRequest :: forall body. DecodeResponse body => Request -> Aff (ClientResponse body)
makeRequest { method, url, body, headers, opts, reqOpts } = do
  res <- AX.request req
  pure (decodeAffjaxResponse res)
  where
  defaultReq =
    AX.defaultRequest
      { method = Left method
      , url = url
      , content = body
      , responseFormat = ResponseFormat.string
      , headers = AX.defaultRequest.headers <> headers
      }
  req = appendHeaders (opts.extraHeaders <> reqOpts.extraHeaders) defaultReq

lookupHeader :: String -> Array ResponseHeader -> Maybe String
lookupHeader _ headers = Array.findMap matchingHeaderVal headers
  where
  matchingHeaderVal :: ResponseHeader -> Maybe String
  matchingHeaderVal (ResponseHeader k val)
    | k == "content-type" = Just val
    | otherwise = Nothing

decodeAffjaxResponse ::
  forall body.
  DecodeResponse body =>
  Either AX.Error (AX.Response String) ->
  ClientResponse body
decodeAffjaxResponse (Left err) = Left (RequestError { message: AX.printError err })
decodeAffjaxResponse (Right res@{ status: StatusCode s }) =
  if s >= 200 && s < 300 
  then do 
    case decodeResponse (StringBody res.body) of
      Right decoded -> Right (bodyResponse res decoded)
      Left error -> Left $ DecodeError { error, response: asPayloadResponse res }
  else Left (StatusError { response: asPayloadResponse res })

bodyResponse :: forall a. AX.Response String -> a -> Response a
bodyResponse res body = Response (Record.insert (Proxy :: _ "body") body rest)
  where
  rest = statusAndHeaders res

asPayloadResponse ::
  AX.Response String ->
  Response String

asPayloadResponse res = Response (Record.insert (Proxy :: _ "body") res.body rest)
  where
  rest = statusAndHeaders res

statusAndHeaders ::
  forall a.
  AX.Response a ->
  { status :: HttpStatus, headers :: Headers }

statusAndHeaders res = { status, headers }
  where
  status = { code: unwrapStatus res.status, reason: res.statusText }
  headers = Headers.fromFoldable (asHeaderTuple <$> res.headers)
  unwrapStatus (StatusCode code) = code

asHeaderTuple :: ResponseHeader -> Tuple String String
asHeaderTuple (ResponseHeader name value) = Tuple name value

encodeUrl ::
  forall url params.
  PayloadUrl.EncodeUrl url params =>
  Options -> Proxy url -> Record params -> Maybe String

encodeUrl opts url params = do 
  path <- PayloadUrl.encodeUrl url params
  let 
    baseUrl = stripTrailingSlash opts.baseUrl
  Just $ baseUrl <> path

stripTrailingSlash :: String -> String
stripTrailingSlash s = fromMaybe s $ String.stripSuffix (String.Pattern "/") s

appendHeaders :: forall a. Headers -> AX.Request a -> AX.Request a
appendHeaders headers req = req { headers = newHeaders }
  where
  newHeaders = req.headers <> (asAxHeader <$> Headers.toUnfoldable headers)

  asAxHeader :: Tuple String String -> RequestHeader
  asAxHeader (Tuple key val) = RequestHeader key val

instance queryableGetRoute ::
  ( Row.Lacks "body" route
  , Row.Union route DefaultRouteSpec mergedRoute
  , Row.Nub mergedRoute routeWithDefaults
  , TypeEquals (Record routeWithDefaults)
      { response :: res
      , params :: Record params
      , query :: query
      | r
      }
  , Row.Union baseParams params fullUrlParams
  , Symbol.Append basePath path fullPath
  , RowToList fullUrlParams fullParamsList
  , EncodeUrlWithParams fullPath fullParamsList payload
  , EncodeOptionalQuery fullPath query payload
  , DecodeResponse res
  ) =>
  Queryable (Route "GET" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts reqOpts payload = do
    let
      encodedUrlPath = encodeUrlWithParams opts (Proxy :: _ fullPath) (Proxy :: _ fullParamsList) payload
    case encodedUrlPath of 
      Just urlPath -> do
        let 
          urlQuery =
            encodeOptionalQuery (Proxy :: _ fullPath)
              (Proxy :: _ query)
              payload
          url = urlPath <> urlQuery
        makeRequest { method: GET, url, body: Nothing, headers: [], opts, reqOpts }
      Nothing ->  pure $ Left URIEncodingError

else instance queryablePostRoute ::
  ( Row.Union route DefaultRouteSpec mergedRoute
  , Row.Nub mergedRoute routeWithDefaults
  , TypeEquals (Record routeWithDefaults)
      { response :: res
      , params :: Record params
      , query :: query
      , body :: body
      | r
      }
  , Row.Union baseParams params fullUrlParams
  , Symbol.Append basePath path fullPath
  , RowToList fullUrlParams fullParamsList
  , EncodeUrlWithParams fullPath fullParamsList payload
  , EncodeOptionalQuery fullPath query payload
  , EncodeOptionalBody body payload
  , HasContentType body
  , DecodeResponse res
  ) =>
  Queryable (Route "POST" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts reqOpts payload = do
    let
      encodedUrlPath = encodeUrlWithParams opts (Proxy :: _ fullPath) (Proxy :: _ fullParamsList) payload
    case encodedUrlPath of 
      Just urlPath -> do 
        let 
          urlQuery =
            encodeOptionalQuery (Proxy :: _ fullPath)
              (Proxy :: _ query)
              payload
          url = urlPath <> urlQuery
          body = encodeOptionalBody (Proxy :: _ body) payload
          headers = maybe [] (\_ -> [ ContentType (MediaType (getContentType (Proxy :: _ body))) ]) body
        makeRequest { method: POST, url, body, headers, opts, reqOpts }
      Nothing -> pure $ Left URIEncodingError

else instance queryableHeadRoute ::    
  ( Row.Lacks "body" route
  , Row.Lacks "response" route
  , Row.Union route DefaultRouteSpec mergedRoute
  , Row.Nub mergedRoute routeWithDefaults
  , TypeEquals (Record routeWithDefaults)
      { params :: Record params
      , query :: query
      | r
      }
  , Symbol.Append basePath path fullPath
  , Row.Union baseParams params fullUrlParams
  , RowToList fullUrlParams fullParamsList
  , EncodeUrlWithParams fullPath fullParamsList payload
  , EncodeOptionalQuery fullPath query payload
  ) =>
  Queryable (Route "HEAD" path (Record route)) basePath baseParams (Record payload) String where
  request _ _ _ opts reqOpts payload = do
    let
      encodedUrlPath = encodeUrlWithParams opts (Proxy :: _ fullPath) (Proxy :: _ fullParamsList) payload
    case encodedUrlPath of 
      Just urlPath -> do
        let
          urlQuery =
            encodeOptionalQuery (Proxy :: _ fullPath)
              (Proxy :: _ query)
              payload
          url = urlPath <> urlQuery
        makeRequest { method: HEAD, url, body: Nothing, headers: [], opts, reqOpts }
      Nothing -> pure $ Left URIEncodingError

else instance queryablePutRoute ::
  ( Row.Union route DefaultRouteSpec mergedRoute
  , Row.Nub mergedRoute routeWithDefaults
  , TypeEquals (Record routeWithDefaults)
      { response :: res
      , params :: Record params
      , query :: query
      , body :: body
      | r
      }
  , Row.Union baseParams params fullUrlParams
  , Symbol.Append basePath path fullPath
  , RowToList fullUrlParams fullParamsList
  , EncodeUrlWithParams fullPath fullParamsList payload
  , EncodeOptionalQuery fullPath query payload
  , EncodeOptionalBody body payload
  , HasContentType body
  , DecodeResponse res
  ) =>
  Queryable (Route "PUT" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts reqOpts payload = do
    let
      encodedUrlPath = encodeUrlWithParams opts (Proxy :: _ fullPath) (Proxy :: _ fullParamsList) payload
    case encodedUrlPath of 
      Just urlPath -> do 
        let 
          urlQuery =
            encodeOptionalQuery (Proxy :: _ fullPath)
              (Proxy :: _ query)
              payload
          url = urlPath <> urlQuery
          body = encodeOptionalBody (Proxy :: _ body) payload
          headers = maybe [] (\_ -> [ ContentType (MediaType (getContentType (Proxy :: _ body))) ]) body
        makeRequest { method: PUT, url, body, headers, opts, reqOpts }
      Nothing -> pure $ Left URIEncodingError

else instance queryableDeleteRoute ::
  ( Row.Union route DefaultRouteSpec mergedRoute
  , Row.Nub mergedRoute routeWithDefaults
  , TypeEquals (Record routeWithDefaults)
      { response :: res
      , params :: Record params
      , query :: query
      , body :: body
      | r
      }
  , Row.Union baseParams params fullUrlParams
  , Symbol.Append basePath path fullPath
  , DecodeResponse res
  , RowToList fullUrlParams fullParamsList
  , EncodeUrlWithParams fullPath fullParamsList payload
  , EncodeOptionalQuery fullPath query payload
  , EncodeOptionalBody body payload
  , HasContentType body
  ) =>
  Queryable (Route "DELETE" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts reqOpts payload = do
    let 
      body = encodeOptionalBody (Proxy :: _ body) payload
      urlQuery = encodeOptionalQuery (Proxy :: _ fullPath) (Proxy :: _ query) payload    
      encodedUrlPath = encodeUrlWithParams opts (Proxy :: _ fullPath) (Proxy :: _ fullParamsList) payload
    case encodedUrlPath of 
      Just urlPath -> do 
        let 
          url = urlPath <> urlQuery
          headers = maybe [] (\_ -> [ ContentType (MediaType (getContentType (Proxy :: _ body))) ]) body
        makeRequest { method: DELETE, url, body, headers, opts, reqOpts }
      Nothing -> pure $ Left URIEncodingError

else instance queryableOptionsRoute ::
  ( Row.Lacks "body" route
  , Row.Union route DefaultRouteSpec mergedRoute
  , Row.Nub mergedRoute routeWithDefaults
  , TypeEquals (Record routeWithDefaults)
      { response :: res
      , params :: Record params
      , query :: query
      | r
      }
  , Row.Union baseParams params fullUrlParams
  , Symbol.Append basePath path fullPath
  , RowToList fullUrlParams fullParamsList
  , EncodeUrlWithParams fullPath fullParamsList payload
  , EncodeOptionalQuery fullPath query payload
  , DecodeResponse res
  ) =>
  Queryable (Route "OPTIONS" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts reqOpts payload = do
    let
      encodedUrlPath = encodeUrlWithParams opts (Proxy :: _ fullPath) (Proxy :: _ fullParamsList) payload
      urlQuery =
        encodeOptionalQuery (Proxy :: _ fullPath)
          (Proxy :: _ query)
          payload
    case encodedUrlPath of 
      Just urlPath -> do
        let url = urlPath <> urlQuery
        makeRequest { method: OPTIONS, url, body: Nothing, headers: [], opts, reqOpts }
      Nothing -> pure $ Left URIEncodingError

class EncodeOptionalBody (body :: Type) (payload :: Row Type) where
  encodeOptionalBody ::
    Proxy body ->
    Record payload ->
    Maybe RequestBody.RequestBody

instance encodeOptionalBodyUndefined :: EncodeOptionalBody Undefined payload where
  encodeOptionalBody _ _ = Nothing
else instance encodeOptionalBodyDefined ::
  ( TypeEquals (Record payload) { body :: body | rest }
  , EncodeBody body
  ) =>
  EncodeOptionalBody body payload where
  encodeOptionalBody _ payload = Just $ RequestBody.String $ encodeBody (to payload).body

class EncodeOptionalQuery (url :: Symbol) (query :: Type) (payload :: Row Type) where
  encodeOptionalQuery ::
    Proxy url ->
    Proxy query ->
    Record payload ->
    String

-- Still need to encode here in case of query literals
instance encodeOptionalQueryUndefined ::
  ( EncodeQuery url ()
  ) =>
  EncodeOptionalQuery url Undefined payload where
  encodeOptionalQuery url _ _ = encodeQuery url {}
else instance encodeOptionalQueryDefined ::
  ( TypeEquals (Record payload) { query :: Record query | rest }
  , EncodeQuery url query
  ) =>
  EncodeOptionalQuery url (Record query) payload where
  encodeOptionalQuery url _ payload = encodeQuery url (to payload).query

class EncodeUrlWithParams (url :: Symbol) (params :: RowList Type) (payload :: Row Type) where
  encodeUrlWithParams ::
    Options ->
    Proxy url ->
    Proxy params ->
    Record payload ->
    Maybe String

instance encodeUrlWithParamsUndefined ::
  ( PayloadUrl.EncodeUrl url ()
  ) =>
  EncodeUrlWithParams url RowList.Nil payload where
  encodeUrlWithParams options url _ _ = encodeUrl options url {}

else instance encodeUrlWithParamsDefined ::
  ( TypeEquals (Record payload) { params :: Record params | rest }
  , ListToRow rl params
  , PayloadUrl.EncodeUrl url params
  ) =>
  EncodeUrlWithParams url rl payload where
  encodeUrlWithParams options url _ payload = encodeUrl options url (to payload).params

