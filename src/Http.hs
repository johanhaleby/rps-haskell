{-# LANGUAGE OverloadedStrings #-}
module Http (runApp, app) where

import           Data.Aeson  (Value (..), object, (.=))
import           Network.Wai (Application)
import qualified Web.Scotty  as S

app' :: S.ScottyM ()
app' = do
  S.get "/" $ do
    S.setHeader "Content-Type" "text/html"
    S.file "./static/index.html"
  S.get "/some-json" $ S.json $ object ["foo" .= Number 23, "bar" .= Number 42]

app :: IO Application
app = S.scottyApp app'

runApp :: IO ()
runApp = S.scotty 8080 app'