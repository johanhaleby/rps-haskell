module Domain( Move(Rock, Paper, Scissors), play, Result(Winner, Tie), State(Ongoing, Ended), Game(Game), PlayerId, PlayerMove(PlayerMove)
  , playerId, move, startGame, state, gameId, firstMove, secondMove, result, GameId, GameRepository(save, findById)) where

import           Data.GUID

-- Domain model

data Move
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Show)

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

-- Game rules
beats :: Move -> Move -> Bool
Rock `beats` Scissors = True
Paper `beats` Rock = True
Scissors `beats` Paper = True
_ `beats` _ = False

playRound :: PlayerMove -> PlayerMove -> Result
playRound PlayerMove {playerId = player1, move = player1Move} PlayerMove {playerId = playerId2, move = player2Move}
  | player1Move == player2Move = Tie
  | player1Move `beats` player2Move = Winner player1
  | otherwise = Winner playerId2
  
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
class GameRepository impl where
   findById :: impl -> GameId -> IO (Maybe Game)
   save :: impl -> Game -> IO Game
