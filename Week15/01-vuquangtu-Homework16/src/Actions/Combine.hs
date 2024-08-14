module Actions.Combine where

import           User.Types

import           Data.List  (tails, transpose)

combinations :: Int -> [Maybe Player] -> Board
combinations n xs = [take n t | t <- tails xs, length t >= n]

diagonals :: Board -> Board
diagonals [] = []
diagonals board = [diag k board | k <- [0 .. (m + n - 2)]]
  where
    m = length board
    n = length (head board)

diag :: Int -> Board -> [Maybe Player]
diag k board =
  [board !! i !! (k - i) | i <- [max 0 (k - n + 1) .. min k (m - 1)]]
  where
    m = length board
    n = length (head board)

getThreeCombinations :: Board -> Board
getThreeCombinations board = concat [rowComb, colComb, diagComb, antiDiagComb]
  where
    rowComb = concatMap (combinations 3) board
    colComb = concatMap (combinations 3) (transpose board)
    diagComb = concatMap (combinations 3) (diagonals board)
    antiDiagComb = concatMap (combinations 3) (diagonals (reverse board))
