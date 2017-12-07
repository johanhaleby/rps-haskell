{-# LANGUAGE OverloadedStrings #-}

module InMemoryRepositorySpec(main, spec) where

import           Control.Monad.IO.Class
import           Data.IORef             (writeIORef)
import           Data.Set               (empty)
import           Domain
import           InMemoryRepository     (clearState, newEmptyRepository)
import           Test.Hspec

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

repository = newEmptyRepository

player1Move = PlayerMove {playerId = "player1", move = Rock}

spec :: Spec
spec =
  before_ (clearState repository) $
  describe "save" $
  context "when game is not started" $
  it "then starting a new game persists the move of player1" $ do
    newGame <- startGame repository player1Move
    newGame `shouldBe` Game {gameId = gameId newGame, state = Ongoing, firstMove = player1Move, secondMove = Nothing, result = Nothing}