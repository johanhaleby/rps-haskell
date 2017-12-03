{-# LANGUAGE OverloadedStrings #-}
module Http (runApp, app) where

import           Data.Aeson                           (Value (..), object, (.=))
import           Network.Wai                          (Application)
import           Network.Wai.Middleware.RequestLogger
import           Web.Scotty

indexPage = do
  setHeader "Content-Type" "text/html"
  file "./static/index.html"

routes :: ScottyM ()
routes = do
  get "/" indexPage
  get "/some-json" $ json $ object ["foo" .= Number 23, "bar" .= Number 42]

app :: IO Application
app = scottyApp routes

runApp :: IO ()
runApp =
  scotty 8080 $ do
    -- Apply request logging
    middleware logStdoutDev
    -- Apply the routes
    routes
