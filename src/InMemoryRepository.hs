{-# LANGUAGE FlexibleInstances #-}

module InMemoryRepository(InMemoryGameRepository(..), clearState, newEmptyRepository) where

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
saveGameToIORef :: IORefState -> Game -> IO Game
saveGameToIORef ioRef game = do
  atomicModifyIORef
    ioRef
    (\games ->
       let newGameState = insert game games
       in (newGameState, newGameState))
  return game

findGameInIORef :: IORefState -> GameId -> IO (Maybe Game)
findGameInIORef ioRef soughtGameId = do
  gamesState <- readIORef ioRef :: IO State
  return $ find (\game -> gameId game == soughtGameId) gamesState

-- Public functions and types

clearState :: InMemoryGameRepository -> IO ()
clearState (InMemoryGameRepository ioRef) = atomicWriteIORef ioRef empty

newEmptyRepository :: IO InMemoryGameRepository
newEmptyRepository = do
  ioRefWithEmptyState <- newIORef empty
  return $ InMemoryGameRepository ioRefWithEmptyState

newtype InMemoryGameRepository = InMemoryGameRepository IORefState

instance GameRepository InMemoryGameRepository where
  findById (InMemoryGameRepository ioRef) = findGameInIORef ioRef
  save (InMemoryGameRepository ioRef) = saveGameToIORef ioRef

doStuff game = do
  ikk <- newIORef empty :: IO IORefState
  let repo = InMemoryGameRepository ikk
  findById repo game