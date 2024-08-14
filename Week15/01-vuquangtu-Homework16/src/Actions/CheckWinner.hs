module Actions.CheckWinner where

import           Actions.Combine
import           Data.Maybe      (listToMaybe, mapMaybe)
import           User.Types

checkWinner :: Board -> Maybe Player
checkWinner board = listToMaybe $ mapMaybe checkLine linesToCheck
  where
    linesToCheck = getThreeCombinations board
    checkLine :: [Maybe Player] -> Maybe Player
    checkLine lineBoard = getJustPlayer line3s
      where
        line3s = combinations 3 lineBoard
        getJustPlayer :: [[Maybe Player]] -> Maybe Player
        getJustPlayer [] = Nothing
        getJustPlayer (x:xs) =
          case isWinningLine x of
            Nothing     -> getJustPlayer xs
            Just player -> Just player
          where
            isWinningLine :: [Maybe Player] -> Maybe Player
            isWinningLine [Just p, Just q, Just r]
              | p == q && q == r = Just p
            isWinningLine _ = Nothing
