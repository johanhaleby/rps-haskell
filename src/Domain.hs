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

data Result
  = Winner PlayerId
  | Tie
  deriving (Eq, Show)

type PlayerId = Integer

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
playRound PlayerMove {playerId = playerId1, move = move1} PlayerMove {playerId = playerId2, move = move2} =
  case moves of
    (Rock, Scissors)  -> Winner playerId1
    (Paper, Rock)     -> Winner playerId1
    (Scissors, Paper) -> Winner playerId1
    (Scissors, Rock)  -> Winner playerId2
    (Rock, Paper)     -> Winner playerId2
    (Paper, Scissors) -> Winner playerId2
    _                 -> Tie
  where
    moves = (move1, move2)

play :: Game -> Game
play g =
  case g of
    Game {state = Ongoing, firstMove = move1, secondMove = Just move2, result = Nothing} -> g {state = Ended, result = Just $ playRound move1 move2}
    _ -> g

newGame :: PlayerMove -> Game
newGame pm = Game {state = Ongoing, firstMove = pm, secondMove = Nothing, result = Nothing}
