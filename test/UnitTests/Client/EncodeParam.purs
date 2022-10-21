module Test.UnitTests.Client.EncodeParam where

import Prelude
import Data.Either (Either(..))
import Payload.Client.EncodeParam (encodeParam)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests =
  suite "URL param encoding" do
    suite "EncodeParam" do
      suite "EncodeParam Int" do
        test "encodes Int"
          $ do
              Assert.equal (Right "1") (encodeParam 1)
      suite "EncodeParam String" do
        test "encodes and URL encodes String"
          $ do
              Assert.equal (Right "john%20doe") (encodeParam "john doe")
