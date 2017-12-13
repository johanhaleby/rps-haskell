{-# LANGUAGE OverloadedStrings #-}

module InMemoryRepositorySpec(main, spec) where

import           Control.Monad.IO.Class
import           Data.IORef             (newIORef)
import           Domain
import           InMemoryRepository
import           Test.Hspec

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  let repositoryIO = newEmptyRepository :: IO InMemoryGameRepository
      player1Move = PlayerMove {playerId = "player1", move = Rock}
  in describe "save" $ do
       context "when game is not started" $
         it "starting a new game persists the move of player1" $ do
           repo <- repositoryIO
           newGame <- startGame repo player1Move
           newGame `shouldBe`
             Game {gameId = gameId newGame, state = Ongoing, firstMove = player1Move, secondMove = Nothing, result = Nothing}
       context "when game is started" $ do
         it "it's possible to find an existing game" $ do
           repo <- repositoryIO
           existingGame <- startGame repo player1Move
           foundGame <- findById repo $ gameId existingGame
           foundGame `shouldBe` Just existingGame
         it "trying to find a non existing game returns Nothing" $ do
           repo <- repositoryIO
           _ <- startGame repo player1Move
           noGame <- findById repo "non-existing"
           noGame `shouldBe` Nothing
       context "when multple games are started" $
         it "it's possible to find all ongoing games" $ do
           repo <- repositoryIO
           startGame repo player1Move
           startGame repo PlayerMove {playerId = "player2", move = Rock}
           findAll repo >>= (`shouldSatisfy` (\games -> length games == 2))
