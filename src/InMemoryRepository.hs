{-# LANGUAGE FlexibleInstances #-}

module InMemoryRepository(InMemoryGameRepository, clearState, newEmptyRepository) where

import           Control.Monad          (mapM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable          (find)
import           Data.Hashable
import           Data.IORef             (IORef, atomicModifyIORef, newIORef,
                                         readIORef, atomicWriteIORef)
import           Data.Set
import           Domain                 (Game, GameId,
                                         GameRepository (findById, save),
                                         gameId)

-- Make Game ordable so that we can insert it to a Set
instance Ord Game where
  compare x y = gameId x `compare` gameId y

type State = (Set Game)
type IORefState = IORef State

-- Internal functions
saveGameToRef :: IO IORefState -> Game -> IO Game
saveGameToRef stateIO game = do
  state <- stateIO
  atomicModifyIORef
    state
    (\games ->
       let newGameState = insert game games
       in (newGameState, newGameState))
  return game

findGameFromRef :: IO IORefState -> GameId -> IO (Maybe Game)
findGameFromRef stateIO soughtGameId = do
  state <- stateIO :: IO IORefState
  gamesState <- readIORef state :: IO State
  return $ find (\game -> gameId game == soughtGameId) gamesState

-- Public functions and types

clearState :: InMemoryGameRepository -> IO ()
clearState (InMemoryGameRepository stateIO) = do
  ioRef <- stateIO
  atomicWriteIORef ioRef empty

newEmptyRepository :: InMemoryGameRepository
newEmptyRepository = InMemoryGameRepository $ newIORef empty

newtype InMemoryGameRepository = InMemoryGameRepository (IO IORefState)

instance GameRepository InMemoryGameRepository where
  findById (InMemoryGameRepository stateIO) = findGameFromRef stateIO
  save (InMemoryGameRepository stateIO) = saveGameToRef stateIO