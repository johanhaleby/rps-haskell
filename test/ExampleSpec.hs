{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module ExampleSpec (main, spec) where

import           Data.Aeson                (Value (..), object, (.=))
import           Network.HTTP.Types.Header
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import           Example                   (app)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  with app $ do
    describe "GET /" $ do
      it "responds with 200" $ get "/" `shouldRespondWith` 200
      it "responds with 'hello'" $ get "/" `shouldRespondWith` "hello"
      it "responds with 200 / 'hello'" $ get "/" `shouldRespondWith` "hello" {matchStatus = 200}
      it "has 'Content-Type: text/plain; charset=utf-8'" $
        get "/" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}
    describe "GET /some-json" $
      it "responds with some JSON" $
        get "/some-json" `shouldRespondWith` expectedJsonResponse

expectedJsonResponse =
  let ResponseMatcher status headers body = [json|{foo: 23, bar: 42}|]
  in ResponseMatcher status [hContentType <:> "application/json; charset=utf-8"] body