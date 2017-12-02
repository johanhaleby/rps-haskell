{-# LANGUAGE OverloadedStrings #-}
module DomainSpec (main, spec) where

import           Domain
import           Control.Monad.IO.Class
import           Test.Hspec

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

initGame :: Move -> Game
initGame m = newGame PlayerMove {playerId = 1, move = m}

makeMove :: Game -> Move -> Game
makeMove g m = g {secondMove = Just PlayerMove {playerId = 2, move = m}}

gameWith :: Move -> Move -> Result -> Game
gameWith m1 m2 r =
  Game
  { state = Ended
  , firstMove = PlayerMove {playerId = 1, move = m1}
  , secondMove = Just PlayerMove {playerId = 2, move = m2}
  , result = Just r
  }
  
spec :: Spec
spec = describe "play" $ do
        context "when game only has one move" $
            it "doesn't change game" $
              play (initGame Rock) `shouldBe` initGame Rock
        context "when game has two players" $ do
          context "and first player plays rock" $ do
              let rockGame = initGame Rock
              it "wins over scissors" $
                play (makeMove rockGame Scissors) `shouldBe` gameWith Rock Scissors (Winner 1)
              it "looses to paper" $
                play (makeMove rockGame Paper) `shouldBe` gameWith Rock Paper (Winner 2)
              it "ties with rock" $
                play (makeMove rockGame Rock) `shouldBe` gameWith Rock Rock Tie
          context "and first player plays paper" $ do
              let paperGame = initGame Paper
              it "looses to scissors" $
                play (makeMove paperGame Scissors) `shouldBe` gameWith Paper Scissors (Winner 2)
              it "ties with paper" $
                play (makeMove paperGame Paper) `shouldBe` gameWith Paper Paper Tie
              it "wins over rock" $
                play (makeMove paperGame Rock) `shouldBe` gameWith Paper Rock (Winner 1)
          context "and first player plays scissors" $ do
              let scissorsGame = initGame Scissors
              it "ties with scissors" $
                play (makeMove scissorsGame Scissors) `shouldBe` gameWith Scissors Scissors Tie
              it "wins over paper" $
                play (makeMove scissorsGame Paper) `shouldBe` gameWith Scissors Paper (Winner 1)
              it "looses to rock" $
                play (makeMove scissorsGame Rock) `shouldBe` gameWith Scissors Rock (Winner 2)
