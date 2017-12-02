{-# LANGUAGE OverloadedStrings #-}
module Example (runApp, app) where

import           Data.Aeson  (Value (..), object, (.=))
import           Network.Wai (Application)
import qualified Web.Scotty  as S

app' :: S.ScottyM ()
app' = do
  S.get "/" $ S.text "hello"
  S.get "/some-json" $ S.json $ object ["foo" .= Number 23, "bar" .= Number 42]

app :: IO Application
app = S.scottyApp app'

runApp :: IO ()
runApp = S.scotty 8080 app'