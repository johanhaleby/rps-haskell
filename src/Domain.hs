{-# LANGUAGE OverloadedStrings #-}

module Domain
  ( Move(Rock, Paper, Scissors)
  , play
  , Result(Winner, Tie)
  , State(Ongoing, Ended)
  , Game(Game)
  , PlayerId
  , PlayerMove(PlayerMove)
  , playerId
  , move
  , newGame
  , state
  , firstMove
  , secondMove
  , result
  ) where

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

data Game = Game
  { state      :: State
  , firstMove  :: PlayerMove
  , secondMove :: Maybe PlayerMove
  , result     :: Maybe Result
  } deriving (Eq, Show)

-- Functions
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

newGame :: PlayerMove -> Game
newGame pm = Game {state = Ongoing, firstMove = pm, secondMove = Nothing, result = Nothing}
