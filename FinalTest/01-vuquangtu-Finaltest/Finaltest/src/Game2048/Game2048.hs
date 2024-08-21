module Game2048.Game2048 where

import           Control.Monad       (when)
import           Data.List

import           System.Console.ANSI (Color (..), ColorIntensity (..),
                                      ConsoleLayer (..), SGR (..),
                                      clearFromCursorToScreenEnd, clearScreen,
                                      setCursorPosition)
import           System.IO           (BufferMode (NoBuffering), hSetBuffering,
                                      stdin)
import           System.Random       (randomRIO)
import           Utility.Color

type Grid = [[Int]]

type Pos = (Int, Int)

targetNum :: Int
targetNum = 2048

emptyGrid :: Grid
emptyGrid = replicate 4 (replicate 4 0)

addRandomTile :: (Grid, Pos) -> IO (Grid, Pos)
addRandomTile (grid, pos) = do
  let emptyCells =
        [(r, c) | r <- [0 .. 3], c <- [0 .. 3], (grid !! r !! c) == 0]
  if null emptyCells
    then return (grid, pos)
    else do
      (r, c) <- randomElement emptyCells
      value <- randomRIO (1, 4) :: IO Int
      let newValue =
            if value == 1
              then 2
              else 4
      return (updateGrid (r, c) newValue grid, (r, c))

randomElement :: [a] -> IO a
randomElement xs = do
  let maxIndex = length xs - 1
  index <- randomRIO (0, maxIndex) :: IO Int
  return (xs !! index)

updateGrid :: (Int, Int) -> Int -> Grid -> Grid
updateGrid (r, c) value grid =
  take r grid
    ++ [take c (grid !! r) ++ [value] ++ drop (c + 1) (grid !! r)]
    ++ drop (r + 1) grid

compressRow :: [Int] -> [Int]
compressRow row =
  let merge1 = merge (filter (/= 0) row)
      merge2 = replicate (4 - length merge1) 0
   in merge1 ++ merge2

merge :: [Int] -> [Int]
merge [] = []
merge [x] = [x]
merge (x:y:xs)
  | x == y = (x * 2) : merge xs
  | otherwise = x : merge (y : xs)

moveLeft :: Grid -> Grid
moveLeft = map compressRow

moveUp :: Grid -> Grid
moveUp = transpose . moveLeft . transpose

moveDown :: Grid -> Grid
moveDown = transpose . moveRight . transpose

moveRight :: Grid -> Grid
moveRight = map reverse . moveLeft . map reverse

gameOver :: Grid -> Bool
gameOver grid = not (canMove grid) && not (has0 grid)

gameWin :: Grid -> Bool
gameWin = any (elem targetNum)

has0 :: Grid -> Bool
has0 = any (elem 0)

canMove :: Grid -> Bool
canMove grid = any canMergeRow grid || any canMergeRow (transpose grid)

canMergeRow :: [Int] -> Bool
canMergeRow []       = False
canMergeRow [_]      = False
canMergeRow (x:y:xs) = x == y || canMergeRow (y : xs)

maxWidth :: Grid -> Int
maxWidth grid = maximum $ map (length . show) (concat grid)

formatNumber :: Int -> Int -> Pos -> Pos -> String
formatNumber width num (r, c) pos =
  let numStr = show num
   in if (r, c) == pos
        then replicate (width - length numStr) ' ' ++ "[" ++ numStr ++ "]"
        else replicate (width - length numStr) ' ' ++ " " ++ numStr ++ " "

printGrid :: (Grid, Pos) -> IO ()
printGrid (grid, pos) = do
  setCursorPosition 2 0
  clearFromCursorToScreenEnd
  let width = maxWidth grid
      formattedRows =
        [ intercalate
          " | "
          [formatNumber width e (r, c) pos | (c, e) <- zip [0 ..] row]
        | (r, row) <- zip [0 ..] grid
        ]
  mapM_ putStrLn formattedRows
  putStrLn "\nPress 'w', 'a', 's', 'd' to move, ESC to exit."

handleInput :: Char -> (Grid, Pos) -> IO (Grid, Pos)
handleInput 'w' (grid, pos) = do
  let newGrid = moveUp grid
  addRandomTile (newGrid, pos)
handleInput 'a' (grid, pos) = do
  let newGrid = moveLeft grid
  addRandomTile (newGrid, pos)
handleInput 's' (grid, pos) = do
  let newGrid = moveDown grid
  addRandomTile (newGrid, pos)
handleInput 'd' (grid, pos) = do
  let newGrid = moveRight grid
  addRandomTile (newGrid, pos)
handleInput _ (grid, pos) = return (grid, pos)

mainLoop :: (Grid, Pos) -> IO ()
mainLoop (grid, pos) = do
  hSetBuffering stdin NoBuffering
  printGrid (grid, pos)
  input <- getChar
  case input of
    '\ESC' -> return ()
    _ -> do
      (newGrid, newPos) <- handleInput input (grid, pos)
      if gameOver newGrid
        then printGrid (newGrid, newPos)
               >> putStrLn "Game Over! No more moves possible."
               >> playAgain
        else if gameWin newGrid
               then putStrLn "You are Winner!!!" >> playAgain
               else mainLoop (newGrid, newPos)

playAgain :: IO ()
playAgain = do
  putStrLn "Do you want to play again?(y,n)"
  confirm <- getChar
  when (confirm == 'y') mainGame2048

mainGame2048 :: IO ()
mainGame2048 = do
  clearScreen
  setCursorPosition 0 0
  putStrLn
    $ setSGRCode [SetColor Foreground Vivid Yellow]
        ++ "2048 GAME"
        ++ setSGRCode [Reset]
  let pos = (4, 4)
  (initialGrid, newPos) <- addRandomTile (emptyGrid, pos) >>= addRandomTile
  mainLoop (initialGrid, newPos)
