module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff as Aff
import Test.Config (defaultConfig)
import Test.IntegrationTests.Client.Errors as ClientErrorsTest
import Test.IntegrationTests.Client.Methods as ClientMethodsTest
import Test.IntegrationTests.Client.Options as ClientOptionsTest
import Test.IntegrationTests.Client.QueryParams as ClientQueryParams
import Test.IntegrationTests.Client.Statuses as ClientStatuses
import Test.IntegrationTests.Client.ContentTypes as ClientContentTypes
import Test.IntegrationTests.Server.Body as BodyTest
import Test.IntegrationTests.Server.Guards as GuardsTest
import Test.IntegrationTests.Server.Methods as MethodsTest
import Test.IntegrationTests.Server.QueryParams as QueryParamsTest
import Test.IntegrationTests.Server.Status as StatusTest
import Test.IntegrationTests.Server.Routing as RoutingTest
import Test.UnitTests.Client.QueryParams as ClientQueryParamsTest
import Test.UnitTests.Client.EncodeParam as ClientEncodeParamTest
import Test.UnitTests.Internal.QueryParsing as QueryParsingTest
import Test.UnitTests.Internal.UrlParsing as UrlParsingTest
import Test.UnitTests.Server.Cookies as CookiesTest
import Test.UnitTests.Server.Internal.OmitEmpty as OmitEmptyTest
import Test.UnitTests.Server.Internal.Query as QueryTest
import Test.UnitTests.Server.Internal.Trie as TrieTest
import Test.UnitTests.Server.Internal.Url as UrlTest
import Test.UnitTests.Server.Params as ParamsTest
import Test.UnitTests.Server.Response as ResponseTest
import Test.Unit (TestSuite, suite)
import Test.Unit.Main (runTestWith)
import Test.Unit.Output.Fancy as Fancy
  
tests :: TestSuite
tests = do
  let cfg = defaultConfig
  suite "Unit - Shared" do
    UrlParsingTest.tests
    QueryParsingTest.tests

  suite "Unit - Server" do
    CookiesTest.tests
    OmitEmptyTest.tests
    ParamsTest.tests
    QueryTest.tests
    ResponseTest.tests
    TrieTest.tests
    UrlTest.tests

  suite "Unit - Client" do
    ClientQueryParamsTest.tests
    ClientEncodeParamTest.tests

  suite "Integration - Server" do
    BodyTest.tests
    MethodsTest.tests
    QueryParamsTest.tests
    GuardsTest.tests
    StatusTest.tests
    RoutingTest.tests

  suite "Integration - Client" do
    ClientErrorsTest.tests cfg
    ClientMethodsTest.tests cfg
    ClientOptionsTest.tests cfg
    ClientQueryParams.tests cfg
    ClientStatuses.tests cfg
    ClientContentTypes.tests cfg

main :: Effect Unit
main = Aff.launchAff_ $ do
  runTestWith Fancy.runTest tests
