{-# LANGUAGE OverloadedStrings #-}
module Http (runApp, app) where

import           Data.Aeson  (Value (..), object, (.=))
import           Network.Wai (Application)
import           Web.Scotty

app' :: ScottyM ()
app' = do
  get "/" $ do
    setHeader "Content-Type" "text/html"
    file "./static/index.html"
  get "/some-json" $ json $ object ["foo" .= Number 23, "bar" .= Number 42]

app :: IO Application
app = scottyApp app'

runApp :: IO ()
runApp = scotty 8080 app'