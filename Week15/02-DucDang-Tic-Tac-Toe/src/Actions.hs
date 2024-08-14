module Actions where

import Data.List (find, transpose, intercalate)
import System.Random

data Player = X | O deriving (Eq, Show)
data Cell = Empty | Player Player deriving (Eq, Show)

type Board = [[Cell]]

initialBoard :: Board
initialBoard = replicate 5 (replicate 5 Empty) -- row col

printBoard :: Board -> IO ()
printBoard board = do
  mapM_ putStrLn (map showRow board)
  where
    showRow row = intercalate "|" $ map showCell row
    showCell Empty = " . "
    showCell (Player X) = " X "
    showCell (Player O) = " O "

-- -- Check if a player won
checkWin :: Board -> Maybe Player
-- checkWin board =
--   let rows = board
--       cols = transpose board
--       diag1 = [board !! i !! i | i <- [0..2]]
--       diag2 = [board !! i !! (2-i) | i <- [0..2]]
--       wlines = rows ++ cols ++ [diag1, diag2]
--       isWin :: Board -> Bool
--       isWin line = all (== head line) line && head line /= [Empty]
--   in find isWin wlines
checkWin = undefined

-- -- Check if it's draw
isDraw :: Board -> Bool
-- isDraw board = all (\row -> all (\cell -> cell /= Empty) row) board && checkWin board == Nothing
isDraw = undefined

-- make a move
makeMove :: Board -> Player -> (Int, Int) -> Either String Board
makeMove board player (row, col)
  | row < 0 || row > 5 || col < 0 || col > 5 = Left "The position is out of scope!"
  | board !! row !! col /= Empty = Left "This position was put before"
  | otherwise = Right $ setCell board row col (Player player)
  where
    setCell board row col cell = take row board ++
                                 [take col (board !! row) ++ [cell] ++ drop (col+1) (board !!row)] ++
                                 drop (row+1) board

-- random move
randomMove :: Board -> IO (Int, Int)
randomMove board = do
  let emptyCells = [(r, c) | r <- [0..4], c <- [0..4], board !! r !! c == Empty]
  if null emptyCells
    then return (-1, -1) -- full board
    else do
      i <- randomRIO (0, length emptyCells - 1)
      return $ emptyCells !! i

playGame :: Player -> Player -> Board -> IO ()
playGame player opponent board = do
  printBoard board
  if checkWin board /= Nothing then
    putStrLn $ "Player " ++ show (checkWin board) ++ " won!"
  else if isDraw board then
    putStrLn "Drawn!"
  else do
    putStrLn $ "the round of " ++ show player
    case player of
      X -> do
        putStrLn "Input row and column (ex: 1 2): "
        input <- getLine
        let [rowStr, colStr] = words input
            row = read rowStr - 1
            col = read colStr - 1
        case makeMove board player (row, col) of
          Left msg -> putStrLn msg >> playGame player opponent board
          Right newBoard -> playGame opponent player newBoard
      O -> do
        (row, col) <- randomMove board
        putStrLn $ "Bot's cell (" ++ show (row+1) ++ ", " ++ show (col+1) ++ ")"
        case makeMove board player (row, col) of
          Left msg -> putStrLn msg >> playGame player opponent board
          Right newBoard -> playGame opponent player newBoard