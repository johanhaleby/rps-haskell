{-# LANGUAGE OverloadedStrings #-}
module Http (runApp, app) where

import           Data.Aeson                           (Value (..), object, (.=))
import           Network.Wai                          (Application)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Web.Scotty

-- The reason why we're not using "staticPolicy" for index.html is that
-- we don't want the user to explicitly have to enter "http://uri/index.html".
-- See https://stackoverflow.com/questions/22662826/web-scotty-file-not-found-while-serving-static-files
indexPage :: ActionM ()
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
    -- Apply static content
    middleware $ staticPolicy noDots
    -- Apply the routes
    routes
