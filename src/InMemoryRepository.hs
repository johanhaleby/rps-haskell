{-# LANGUAGE FlexibleInstances #-}

module InMemoryRepository(InMemoryGameRepository(..), clearState, newEmptyRepository) where

import           Data.HashMap.Lazy      (HashMap (..), empty, insert,
                                         lookup, elems)
import           Data.IORef             (IORef, atomicModifyIORef,
                                         atomicWriteIORef, newIORef, readIORef)
import           Domain                 (Game, GameId,
                                         GameRepository (findById, save, findAll),
                                         gameId)
import           Prelude                hiding (lookup)

type State = (HashMap GameId Game)
type IORefState = IORef State

-- Internal functions
saveGameToIORef :: IORefState -> Game -> IO Game
saveGameToIORef ioRef game = do
  atomicModifyIORef
    ioRef
    (\games ->
       let newGameState = insert (gameId game) game games
       in (newGameState, newGameState))
  return game

findGameInIORef :: IORefState -> GameId -> IO (Maybe Game)
findGameInIORef ioRef soughtGameId = do
  gamesState  <- readIORef ioRef :: IO State
  return $ lookup soughtGameId gamesState

findAllInIORef :: IORefState -> IO [Game]
findAllInIORef ioRef = do
  gamesState <- readIORef ioRef :: IO State
  return $ elems gamesState

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
  findAll (InMemoryGameRepository ioRef) = findAllInIORef ioRef
