module InMemoryRepository where

import           Control.Monad          (mapM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable          (find)
import           Data.Hashable
import           Data.IORef             (IORef, atomicModifyIORef, newIORef,
                                         readIORef)
import           Data.Set
import           Domain                 hiding (State)

-- Make Game ordable so that we can insert it to a Set
instance Ord Game where
  compare x y = gameId x `compare` gameId y

type State = (Set Game)
type IORefState = IORef State

ioRefState :: IO IORefState
ioRefState = newIORef empty

saveGame :: IO IORefState -> Game -> IO Game
saveGame stateIO game = do
  state <- stateIO
  atomicModifyIORef
    state
    (\games ->
       let newGameState = insert game games
       in (newGameState, newGameState))
  return game
findGame :: IO IORefState -> GameId -> IO (Maybe Game)
findGame stateIO id = do
  state <- stateIO :: IO IORefState
  gamesState <- readIORef state :: IO State
  return $ find (\game -> gameId game == id) gamesState

instance GameRepository a =>
         GameRepository (IORef a) where
  findById _ = findGame ioRefState
  save _ = saveGame ioRefState