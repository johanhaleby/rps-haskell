{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Http (runApp, app) where

import           Control.Monad.IO.Class               (liftIO)
import           Data.Aeson                           (FromJSON, ToJSON,
                                                       Value (..), encode,
                                                       object, parseJSON,
                                                       toJSON, withObject,
                                                       withText, (.:), (.=))
-- <> is needed to concatenate Text's
import           Data.Semigroup                       ((<>))
-- Need for converting strict Text (Data.Text) to lazy Text (Data.Text.Lazy) which Scotty is using
import           Data.Text.Lazy                       (fromStrict)
import           Domain
import           InMemoryRepository
import           Network.HTTP.Types                   (created201)
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

routes :: (GameRepository repo) => repo -> ScottyM ()
routes gameRepository = do
  get "/" indexPage
  -- API
  get "/api/games" $ do
    allGames <- liftIO $ findAll gameRepository
    json $ fmap toJSON allGames
  post "/api/games" $ do
    playerMove <- jsonData :: ActionM PlayerMove
    game <- liftIO $ startGame gameRepository playerMove
    setHeader "Location" (fromStrict $ "/api/games/" <> gameId game)
    status created201

app :: IO Application
app = do
  repo <- newEmptyRepository
  scottyApp $ routes repo

runApp :: IO ()
runApp = do
  repo <- newEmptyRepository :: IO InMemoryGameRepository
  scotty 8080 $ do
    -- Apply request logging
    middleware logStdoutDev
    -- Apply static content
    middleware $ staticPolicy noDots
    -- Apply the routes
    routes repo

-- JSON mapping
instance ToJSON Game where
  toJSON Game{..} = object
      [ "gameId"  .= gameId
      , "state"   .= show state
      , "player1" .= playerId firstMove
      ]

instance FromJSON PlayerMove where
  parseJSON =
    withObject "person" $ \o -> do
      playerId <- o .: "playerId"
      move <- o .: "move"
      return PlayerMove {..}

--instance FromJSON Move where
--  parseJSON = withObject "move" $ \o -> do
--      kind <- o .: "kind"
--      case kind of
--        "person" -> Person <$> o .: "name" <*> o .: "age"
--        "book"   -> Book <$> o .: "name" <*> o .: "author"
--        _        -> fail ("unknown kind: " ++ kind)

instance FromJSON Move where
  parseJSON = withText "move" $ \txt ->
    return $ case txt of
      "Rock"     -> Rock
      "Paper"    -> Paper
      "Scissors" -> Scissors
      _          -> Rock
