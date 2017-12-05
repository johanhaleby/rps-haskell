{-# LANGUAGE OverloadedStrings #-}

module Domain( Move(Rock, Paper, Scissors), play, Result(Winner, Tie), State(Ongoing, Ended), Game(Game), PlayerId, PlayerMove(PlayerMove)
  , playerId, move, startGame, state, gameId, firstMove, secondMove, result, GameId, GameRepository(save, findById)) where

import           Data.GUID

-- Domain model

data Move
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Show)

-- Game rules
instance Ord Move where
  (<=) m1 m2 = m1 == m2 || (m1, m2) `elem` [(Rock, Paper), (Paper, Scissors), (Scissors, Rock)]

data Result
  = Winner PlayerId
  | Tie
  deriving (Eq, Show)

type PlayerId = String

data PlayerMove = PlayerMove
  { playerId :: PlayerId
  , move     :: Move
  } deriving (Eq, Show)

data State
  = Ongoing
  | Ended
  deriving (Eq, Show)

type GameId = String

data Game = Game
  { gameId     :: GameId
  , state      :: State
  , firstMove  :: PlayerMove
  , secondMove :: Maybe PlayerMove
  , result     :: Maybe Result
  } deriving (Eq, Show)

-- Functions

generateGameId :: IO GameId
generateGameId = genString

playRound :: PlayerMove -> PlayerMove -> Result
playRound pm1 pm2 =
  case result of
    LT -> Winner $ playerId pm2
    EQ -> Tie
    GT -> Winner $ playerId pm1
  where
    result = move pm1 `compare` move pm2

play :: Game -> Game
play g =
  case g of
    Game {state = Ongoing, firstMove = move1, secondMove = Just move2, result = Nothing} -> g {state = Ended, result = Just $ playRound move1 move2}
    _ -> g

startGame :: (GameRepository repo) => repo -> PlayerMove -> IO Game
startGame repo pm = do
  gameId <- generateGameId
  let game = Game {gameId = gameId, state = Ongoing, firstMove = pm, secondMove = Nothing, result = Nothing}
  repo `save` game

-- Repository
class GameRepository repo where
   findById :: repo -> GameId -> IO (Maybe Game)
   save :: repo -> Game -> IO Game
