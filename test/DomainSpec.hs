{-# LANGUAGE OverloadedStrings #-}
module DomainSpec (main, spec) where

import           Domain
import           Control.Monad.IO.Class
import           Test.Hspec

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

-- Some fixtures
player1 = "1"
player2 = "2"
game = "abc"

-- Init a new game with a given move for player1
makeGame :: Move -> Game
makeGame m =
  Game {state = Ongoing, gameId = game, firstMove = PlayerMove {playerId = player1, move = m}, secondMove = Nothing, result = Nothing}

-- Make a move for player2
makeMove :: Game -> Move -> Game
makeMove g m = g {secondMove = Just PlayerMove {playerId = player2, move = m}}

-- Generate a Game data structure with the given attributes
gameWith :: Move -> Move -> Result -> Game
gameWith m1 m2 r =
  Game
  { gameId = game
  , state = Ended
  , firstMove = PlayerMove {playerId = player1, move = m1}
  , secondMove = Just PlayerMove {playerId = player2, move = m2}
  , result = Just r
  }
  
spec :: Spec
spec = describe "play" $ do
        context "when game only has one move" $
            it "doesn't change game" $
              play (makeGame Rock) `shouldBe` makeGame Rock
        context "when game has two players" $ do
          context "and first player plays rock" $ do
              let rockGame = makeGame Rock
              it "wins over scissors" $
                play (makeMove rockGame Scissors) `shouldBe` gameWith Rock Scissors (Winner player1)
              it "looses to paper" $
                play (makeMove rockGame Paper) `shouldBe` gameWith Rock Paper (Winner player2)
              it "ties with rock" $
                play (makeMove rockGame Rock) `shouldBe` gameWith Rock Rock Tie
          context "and first player plays paper" $ do
              let paperGame = makeGame Paper
              it "looses to scissors" $
                play (makeMove paperGame Scissors) `shouldBe` gameWith Paper Scissors (Winner player2)
              it "ties with paper" $
                play (makeMove paperGame Paper) `shouldBe` gameWith Paper Paper Tie
              it "wins over rock" $
                play (makeMove paperGame Rock) `shouldBe` gameWith Paper Rock (Winner player1)
          context "and first player plays scissors" $ do
              let scissorsGame = makeGame Scissors
              it "ties with scissors" $
                play (makeMove scissorsGame Scissors) `shouldBe` gameWith Scissors Scissors Tie
              it "wins over paper" $
                play (makeMove scissorsGame Paper) `shouldBe` gameWith Scissors Paper (Winner player1)
              it "looses to rock" $
                play (makeMove scissorsGame Rock) `shouldBe` gameWith Scissors Rock (Winner player2)
