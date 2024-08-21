module Game2048.Game2048 where

import           System.Console.ANSI hiding (setSGRCode)

import           Control.Monad       (when)
import           Data.Binary         (decodeFile, encodeFile)
import           Data.List
import           System.Console.ANSI (setSGRCode)
import           System.Directory    (doesFileExist)
import           System.IO           (BufferMode (NoBuffering), hFlush,
                                      hSetBuffering, stdin, stdout)
import           System.Random       (randomRIO)
import           Utility.Color

type Grid = [[Int]]

type Pos = (Int, Int)

type GameState = (Grid, Pos)

-- Hàm để chọn một ô trống ngẫu nhiên và thêm số 2 hoặc 4 vào ô đó
addRandomTile :: GameState -> IO GameState
addRandomTile (grid, pos) = do
  let emptyCells =
        [ (r, c)
        | r <- [0 .. length grid - 1]
        , c <- [0 .. length grid - 1]
        , (grid !! r !! c) == 0
        ]
  if null emptyCells
    then return (grid, pos)
    else do
      (r, c) <- randomElement emptyCells
      value <- randomRIO (1, 4) :: IO Int
      let newValue =
            if value == 1
              then 4
              else 2
      return (updateGrid (r, c) newValue grid, (r, c))

-- Hàm để chọn một phần tử ngẫu nhiên từ danh sách
randomElement :: [a] -> IO a
randomElement xs = do
  let maxIndex = length xs - 1
  index <- randomRIO (0, maxIndex) :: IO Int
  return (xs !! index)

-- Cập nhật giá trị của một ô trong bảng
updateGrid :: (Int, Int) -> Int -> Grid -> Grid
updateGrid (r, c) value grid =
  take r grid
    ++ [take c (grid !! r) ++ [value] ++ drop (c + 1) (grid !! r)]
    ++ drop (r + 1) grid

-- Hàm nén hàng, kết hợp các ô có cùng giá trị
compressRow :: [Int] -> [Int]
compressRow row =
  let nonZeroRow = filter (/= 0) row
      mergedRow = merge nonZeroRow
      numZeros = length row - length mergedRow
   in mergedRow ++ replicate numZeros 0

-- Hàm kết hợp các ô có cùng giá trị
merge :: [Int] -> [Int]
merge [] = []
merge [x] = [x]
merge (x:y:xs)
  | x == y = (x * 2) : merge xs
  | otherwise = x : merge (y : xs)

-- Hàm di chuyển các ô sang trái
moveLeft :: Grid -> Grid
moveLeft = map compressRow

-- Hàm di chuyển các ô lên
moveUp :: Grid -> Grid
moveUp = transpose . moveLeft . transpose

-- Hàm di chuyển các ô xuống
moveDown :: Grid -> Grid
moveDown = transpose . moveRight . transpose

-- Hàm di chuyển các ô sang phải
moveRight :: Grid -> Grid
moveRight = map reverse . moveLeft . map reverse

-- Kiểm tra điều kiện kết thúc trò chơi
gameOver :: Grid -> Bool
gameOver grid = not (canMove grid) && not (any (elem 0) grid)

gameWin :: Int -> Grid -> Bool
gameWin targetNum = any (elem targetNum)

-- Kiểm tra nếu có bất kỳ phép di chuyển hợp lệ nào
canMove :: Grid -> Bool
canMove grid = any canMergeRow grid || any canMergeRow (transpose grid)

-- Kiểm tra nếu hàng có thể hợp nhất
canMergeRow :: [Int] -> Bool
canMergeRow []       = False
canMergeRow [_]      = False
canMergeRow (x:y:xs) = x == y || canMergeRow (y : xs)

-- Tính độ rộng lớn nhất của các số trong grid, bao gồm cả dấu ngoặc vuông nếu cần
maxWidth :: Grid -> Int
maxWidth grid = maximum $ map (length . show) (concat grid)

-- Định dạng một số với độ rộng nhất định và thêm dấu ngoặc vuông nếu vị trí hiện tại là pos
formatNumber :: Int -> Int -> Pos -> Pos -> String
formatNumber width num (r, c) pos =
  let numStr = show num
      color = colorForValue num
   in if (r, c) == pos
        then applyEffects
               [yellowBackground, redText, boldText]
               (replicate (width - length numStr) ' ' ++ "[" ++ numStr ++ "]")
        else setSGRCode color
               ++ replicate (width - length numStr) ' '
               ++ " "
               ++ numStr
               ++ " "
               ++ setSGRCode [Reset]

-- Hiển thị bảng trò chơi với căn chỉnh số
printGrid :: GameState -> IO ()
printGrid (grid, pos) = do
  clearScreen
  setCursorPosition 0 0
  let width = maxWidth grid
      formattedRows =
        [ intercalate
          " | "
          [formatNumber width e (r, c) pos | (c, e) <- zip [0 ..] row]
        | (r, row) <- zip [0 ..] grid
        ]
  mapM_ putStrLn formattedRows
  -- putStrLn
  --   "Press 'w', 'a', 's', 'd' to move, 'u' to undo,'v' to save, 'l' to load prev game. ESC to exit."
  setSGR [SetColor Foreground Vivid Red]
  putStrLn
    "Press 'w', 'a', 's', 'd' to move, 'u' to undo,'v' to save, 'l' to load prev game. ESC to exit."
  setSGR [Reset]

-- Lưu trạng thái hiện tại vào stack lịch sử
saveState :: GameState -> [GameState] -> [GameState]
saveState state history = state : history

-- Xử lý di chuyển và cập nhật bảng
handleInput :: Char -> GameState -> [GameState] -> IO (GameState, [GameState])
handleInput 'w' (grid, pos) history = do
  let newGrid = moveUp grid
  newState <- addRandomTile (newGrid, pos)
  return (newState, saveState newState history)
handleInput 'a' (grid, pos) history = do
  let newGrid = moveLeft grid
  newState <- addRandomTile (newGrid, pos)
  return (newState, saveState newState history)
handleInput 's' (grid, pos) history = do
  let newGrid = moveDown grid
  newState <- addRandomTile (newGrid, pos)
  return (newState, saveState newState history)
handleInput 'd' (grid, pos) history = do
  let newGrid = moveRight grid
  newState <- addRandomTile (newGrid, pos)
  return (newState, saveState newState history)
handleInput 'u' (grid, pos) history = do
  case history of
    (prevState:rest) -> return (prevState, rest)
    []               -> return ((grid, pos), history)
handleInput 'v' (grid, pos) history = do
  putStrLn "\nAre you sure to save game?(y or n)"
  option <- getLine
  if option == "y"
    then do
      saveGameState "savegame.dat" (grid, pos)
      return ((grid, pos), history)
    else return ((grid, pos), history)
handleInput 'l' (grid, pos) history = do
  putStrLn "\nAre you sure to load prev game?(y or n)"
  option <- getLine
  if option == "y"
    then do
      mState <- loadGameState "savegame.dat"
      case mState of
        Just state -> return (state, history)
        Nothing    -> return ((grid, pos), history)
    else return ((grid, pos), history)
handleInput _ state history = return (state, history)

-- Vòng lặp chính của trò chơi
mainLoop :: Int -> GameState -> [GameState] -> IO ()
mainLoop targetNum state history = do
  hSetBuffering stdin NoBuffering
  printGrid state
  input <- getChar
  (newState, newHistory) <- handleInput input state history
  if gameOver (fst newState)
    then printGrid newState
           >> putStrLn "Game Over! No more moves possible."
           >> playAgain
    else if gameWin targetNum (fst newState)
           then printGrid newState
                  >> putStrLn "\nYou are Winner!!!"
                  >> playAgain
           else mainLoop targetNum newState newHistory

-- Lưu trạng thái hiện tại vào file
saveGameState :: FilePath -> GameState -> IO ()
saveGameState = encodeFile

-- Tải trạng thái trò chơi từ file
loadGameState :: FilePath -> IO (Maybe GameState)
loadGameState filename = do
  fileExists <- doesFileExist filename
  if fileExists
    then Just <$> decodeFile filename
    else return Nothing

playAgain :: IO ()
playAgain = do
  putStrLn "Do you want to play again?(y,n)"
  confirm <- getLine
  when (confirm == "y") mainGame2048

-- Hàm chính để khởi tạo trò chơi và bắt đầu vòng lặp
mainGame2048 :: IO ()
mainGame2048 = do
  putStrLn "Enter the size of the grid (e.g., 4 for a 4x4 grid):"
  hFlush stdout
  gridSizeInput <- getLine
  let gridSize = read gridSizeInput :: Int
  putStrLn "Enter the target number to win the game (e.g., 2048):"
  targetNumInput <- getLine
  let targetNum = read targetNumInput :: Int
      pos = (gridSize + 1, gridSize + 1)
      emptyGrid = replicate gridSize (replicate gridSize 0)
  (initialGrid, newPos) <- addRandomTile (emptyGrid, pos) >>= addRandomTile
  mainLoop targetNum (initialGrid, newPos) []
