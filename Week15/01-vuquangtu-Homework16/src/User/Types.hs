module User.Types where

data Player
  = X
  | O
  deriving (Show, Eq)

type Board = [[Maybe Player]]

type Location = (Int, Int)

type Moves = [(Player, Location)]
