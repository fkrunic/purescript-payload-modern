module Payload.Client.Queryable where

import Prelude

import Affjax.Node as AX
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseHeader (ResponseHeader(..))
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except.Trans (ExceptT(..), except, withExceptT)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.MediaType (MediaType(..))
import Data.String (Pattern(..), stripSuffix) as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Payload.Client.DecodeResponse (class DecodeResponse, decodeResponse)
import Payload.Client.EncodeBody (class EncodeBody, encodeBody)
import Payload.Client.Internal.Query (class EncodeQuery, encodeQuery)
import Payload.Client.Internal.Url as PayloadUrl
import Payload.Client.Options (Options, RequestOptions)
import Payload.Client.Response (ClientError(..), ClientResponse)
import Payload.Client.QueryParams (EncodingError(..))
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
  = payload -> ExceptT ClientError Aff (Response body)

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

makeRequest :: forall body. DecodeResponse body => Request -> ExceptT ClientError Aff (Response body)
makeRequest { method, url, body, headers, opts, reqOpts } = do
  res <- withExceptT (\err -> RequestError { message: AX.printError err }) $ ExceptT $ AX.request req
  except $ decodeAffjaxResponse res
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
  AX.Response String ->
  ClientResponse body
decodeAffjaxResponse res@{ status: StatusCode s } =
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
  Options -> 
  Proxy url -> 
  Record params -> 
  Either EncodingError (Maybe String)

encodeUrl opts url params = do 
  encodedPath <- PayloadUrl.encodeUrl url params
  case encodedPath of 
    Just path -> 
      let 
        baseUrl = stripTrailingSlash opts.baseUrl
      in Right $ Just $ baseUrl <> path
    Nothing -> Left EmptyEncoding
  
stripTrailingSlash :: String -> String
stripTrailingSlash s = fromMaybe s $ String.stripSuffix (String.Pattern "/") s

asAxHeader :: Tuple String String -> RequestHeader
asAxHeader (Tuple key val) = RequestHeader key val

appendHeaders :: forall a. Headers -> AX.Request a -> AX.Request a
appendHeaders headers req = req { headers = newHeaders }
  where
  newHeaders = req.headers <> (asAxHeader <$> Headers.toUnfoldable headers)


-- buildURL :: forall fullPath fullParamList query. 
-- buildURL opts payload = do
--   encodedUrlPath <- except $ lmap URIEncodingError $ encodeUrlWithParams opts (Proxy :: _ fullPath) (Proxy :: _ fullParamsList) payload
--   urlQuery <- except $ lmap URIEncodingError $ encodeOptionalQuery (Proxy :: _ fullPath) (Proxy :: _ query) payload      
--   case encodedUrlPath of 
--     Just urlPath -> except $ Right $ urlPath <> urlQuery   
--     Nothing ->  except $ Left $ URIEncodingError EmptyEncoding 

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
    encodedUrlPath <- except $ lmap URIEncodingError $ encodeUrlWithParams opts (Proxy :: _ fullPath) (Proxy :: _ fullParamsList) payload
    urlQuery <- except $ lmap URIEncodingError $ encodeOptionalQuery (Proxy :: _ fullPath) (Proxy :: _ query) payload      
    case encodedUrlPath of 
      Just urlPath -> do
        let 
          url = urlPath <> urlQuery
        makeRequest { method: GET, url, body: Nothing, headers: [], opts, reqOpts }
      Nothing ->  except $ Left $ URIEncodingError EmptyEncoding

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
    encodedUrlPath <- except $ lmap URIEncodingError $ encodeUrlWithParams opts (Proxy :: _ fullPath) (Proxy :: _ fullParamsList) payload
    urlQuery <- except $ lmap URIEncodingError $ encodeOptionalQuery (Proxy :: _ fullPath) (Proxy :: _ query) payload    
    case encodedUrlPath of 
      Just urlPath -> do 
        let 
          url = urlPath <> urlQuery
          body = encodeOptionalBody (Proxy :: _ body) payload
          headers = maybe [] (\_ -> [ ContentType (MediaType (getContentType (Proxy :: _ body))) ]) body
        makeRequest { method: POST, url, body, headers, opts, reqOpts }
      Nothing -> except $ Left $ URIEncodingError EmptyEncoding

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
    encodedUrlPath <- except $ lmap URIEncodingError $ encodeUrlWithParams opts (Proxy :: _ fullPath) (Proxy :: _ fullParamsList) payload
    urlQuery <- except $ lmap URIEncodingError $ encodeOptionalQuery (Proxy :: _ fullPath) (Proxy :: _ query) payload    
    case encodedUrlPath of 
      Just urlPath -> do
        let
          url = urlPath <> urlQuery
        makeRequest { method: HEAD, url, body: Nothing, headers: [], opts, reqOpts }
      Nothing -> except $ Left $ URIEncodingError EmptyEncoding

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
    encodedUrlPath <- except $ lmap URIEncodingError $ encodeUrlWithParams opts (Proxy :: _ fullPath) (Proxy :: _ fullParamsList) payload
    urlQuery <- except $ lmap URIEncodingError $ encodeOptionalQuery (Proxy :: _ fullPath) (Proxy :: _ query) payload    
    case encodedUrlPath of 
      Just urlPath -> do 
        let 
          url = urlPath <> urlQuery
          body = encodeOptionalBody (Proxy :: _ body) payload
          headers = maybe [] (\_ -> [ ContentType (MediaType (getContentType (Proxy :: _ body))) ]) body
        makeRequest { method: PUT, url, body, headers, opts, reqOpts }
      Nothing -> except $ Left $ URIEncodingError EmptyEncoding

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
    urlQuery <- except $ lmap URIEncodingError $ encodeOptionalQuery (Proxy :: _ fullPath) (Proxy :: _ query) payload    
    encodedUrlPath <- except $ lmap URIEncodingError $ encodeUrlWithParams opts (Proxy :: _ fullPath) (Proxy :: _ fullParamsList) payload
    let 
      body = encodeOptionalBody (Proxy :: _ body) payload
    case encodedUrlPath of 
      Just urlPath -> do 
        let 
          url = urlPath <> urlQuery
          headers = maybe [] (\_ -> [ ContentType (MediaType (getContentType (Proxy :: _ body))) ]) body
        makeRequest { method: DELETE, url, body, headers, opts, reqOpts }
      Nothing -> except $ Left $ URIEncodingError EmptyEncoding

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
    encodedUrlPath <- except $ lmap URIEncodingError $ encodeUrlWithParams opts (Proxy :: _ fullPath) (Proxy :: _ fullParamsList) payload
    urlQuery <- except $ lmap URIEncodingError $ encodeOptionalQuery (Proxy :: _ fullPath) (Proxy :: _ query) payload
    case encodedUrlPath of 
      Just urlPath -> do
        let url = urlPath <> urlQuery
        makeRequest { method: OPTIONS, url, body: Nothing, headers: [], opts, reqOpts }
      Nothing -> except $ Left $ URIEncodingError EmptyEncoding

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
    Either EncodingError String

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
    Either EncodingError (Maybe String)

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
