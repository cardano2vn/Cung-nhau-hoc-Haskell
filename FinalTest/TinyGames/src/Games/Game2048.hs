{-# LANGUAGE TypeApplications #-}

module Games.Game2048 where

import           Data.List           (transpose)
import           System.Console.ANSI
import           System.Random       (randomRIO)
import           Text.Printf         (printf)
import           Utils.GameSetting   (boardSize, targetNum)
import           Utils.Helpers       (GameState (..), getCharAndCheckEsc, boldText)

type Row   = [Int]
type Board = [Row]

mergeNumbers :: [Int] -> [Int]
mergeNumbers (x:y:rest)
  | x == y = (2 * x) : mergeNumbers rest
mergeNumbers (x:rest) = x : mergeNumbers rest
mergeNumbers [] = []

padTo :: Int -> [Int] -> Row
padTo n = take n . (++ repeat 0)

processRow :: Row -> Row
processRow = padTo boardSize . mergeNumbers . filter (/= 0)

rotateBoard90 :: Board -> Board
rotateBoard90 = reverse . transpose

displayBoard :: Board -> IO ()
displayBoard board = do
  clearScreen
  setCursorPosition 0 0
  setSGR [SetColor Foreground Vivid Yellow]
  putStrLn $ boldText "2048 Game\n"
  setSGR [Reset]
  putStrLn . unlines . map (concatMap showCell) $ board
  where
    showCell 0 = "     ."
    showCell n = printf "%6d" n

moveBoard :: Board -> Char -> Board
moveBoard board move =
  foldr
    (\dir ->
       if move == dir
         then map processRow . rotateBoard90
         else rotateBoard90)
    board
    "asdw"

addRandomCell :: Board -> IO Board
addRandomCell board = do
  let emptyCells = [(i, j) | i <- [0 .. 3], j <- [0 .. 3], board !! i !! j == 0]
  if null emptyCells
    then return board
    else do
      idx <- randomRIO (0, length emptyCells - 1)
      let (i, j) = emptyCells !! idx
      value <- randomRIO @Int (0, 99)
      let newValue = if value < 75 then 2 else 4
      return $ updateBoard board i j newValue

updateBoard :: Board -> Int -> Int -> Int -> Board
updateBoard board i j value =
  take i board
    ++ [take j (board !! i) ++ [value] ++ drop (j + 1) (board !! i)]
    ++ drop (i + 1) board

isGameOver :: Board -> Bool
isGameOver board = all (\move -> moveBoard board move == board) "aw"

hasEmptyCell :: Board -> Bool
hasEmptyCell = any (elem 0)

gameLoop :: Board -> IO GameState
gameLoop board
  | targetNum `elem` concat board = do
    displayBoard board
    setSGR [SetColor Background Vivid Green]
    putStrLn "You won! You reached 2048!"
    setSGR [Reset]
    return Finish
  | not (hasEmptyCell board) && isGameOver board = do
    displayBoard board
    setSGR [SetColor Background Vivid Red]
    putStrLn "Game Over! No more moves possible."
    setSGR [Reset]
    return Finish
  | otherwise = do
    displayBoard board
    putStr "Press w/a/s/d to move."
    input <- getCharAndCheckEsc
    case input of
      Nothing -> return Exit
      Just move -> do
        if move `elem` "wasd"
          then do
            let movedBoard = moveBoard board move
            if movedBoard /= board
              then do
                newBoard <- addRandomCell movedBoard
                gameLoop newBoard
              else gameLoop board
          else do
            putStrLn "Invalid move! Use w, a, s, or d."
            gameLoop board

game2048 :: IO GameState
game2048 = do
  let emptyBoard = replicate boardSize (replicate boardSize 0)
  tempBoard <- addRandomCell emptyBoard
  initBoard <- addRandomCell tempBoard
  gameLoop initBoard
