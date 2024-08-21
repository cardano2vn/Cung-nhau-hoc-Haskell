import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import System.Random (randomRIO)
import System.Exit (exitSuccess)
import Control.Monad (when, replicateM)
import Data.List (intercalate, transpose, splitAt)
import System.Process (callCommand)
import System.Info (os)

pause :: IO ()
pause = do
  putStrLn "Press Enter to continue..."
  _ <- getLine
  return ()

clearScreen :: IO ()
clearScreen = do
    let command = case os of
                    "mingw32" -> "cls"
                    _         -> "clear"
    callCommand command

-- Trof chơi Hangman
hangman :: IO ()
hangman = do
  words <- loadWords
  secretWord <- getRandomWord words
  gameLoopHangMan secretWord [] 6
  askReplay hangman

loadWords :: IO [String]
loadWords = do
  contents <- readFile "hangman_words.txt"
  return (lines contents)

getRandomWord :: [String] -> IO String
getRandomWord words = do
  index <- randomRIO (0, length words - 1)
  return (words !! index)

gameLoopHangMan :: String -> [Char] -> Int -> IO ()
gameLoopHangMan word guessed remainingGuesses = do
  clearScreen
  let currentStatus = getCurrentStatus word guessed
  putStrLn (displayHangman remainingGuesses)
  putStrLn ("Word: " ++ currentStatus)
  putStrLn ("Remaining guesses: " ++ show remainingGuesses)
  putStrLn ("Guessed letters: " ++ intercalate " " (map return guessed))
  
  if currentStatus == word then
    putStrLn "Congratulations! You've guessed the word!"
  else if remainingGuesses == 0 then
    putStrLn ("Game over! You've been hanged! The word was: " ++ word)
  else do
    putStrLn "Enter your guess (or 'q' to quit):"
    guess <- getValidGuess guessed
    if guess == 'q' then
      putStrLn "Exiting the game..." >> exitSuccess
    else if guess `elem` word then
      gameLoopHangMan word (guess:guessed) remainingGuesses
    else
      gameLoopHangMan word (guess:guessed) (remainingGuesses - 1)

getCurrentStatus :: String -> [Char] -> String
getCurrentStatus word guessed = [if c `elem` guessed then c else '-' | c <- word]

getValidGuess :: [Char] -> IO Char
getValidGuess guessed = do
  guess <- getChar
  _ <- getLine 
  if guess == 'q' then
    return 'q'
  else if not (elem guess ['a'..'z']) then do
    putStrLn "Please enter an alphabet letter."
    getValidGuess guessed
  else if guess `elem` guessed then do
    putStrLn "You've already guessed that letter."
    getValidGuess guessed
  else
    return guess

displayHangman :: Int -> String
displayHangman n = hangmanParts !! (6 - n)
  where
    hangmanParts =
      [ "    ___\n   |  \\|\n       |\n       |\n       |\n       |\n     __|__\n"
      , "    ___\n   |  \\|\n   o   |\n       |\n       |\n       |\n     __|__\n"
      , "    ___\n   |  \\|\n   o   |\n   |   |\n       |\n       |\n     __|__\n"
      , "    ___\n   |  \\|\n   o   |\n  /|   |\n       |\n       |\n     __|__\n"
      , "    ___\n   |  \\|\n   o   |\n  /|\\  |\n       |\n       |\n     __|__\n"
      , "    ___\n   |  \\|\n   o   |\n  /|\\  |\n  /    |\n       |\n     __|__\n"
      , "    ___\n   |  \\|\n   o   |\n  /|\\  |\n  / \\  |\n       |\n     __|__\n"
      ]

-- 2048 Game
type Board = [[Int]]

game2048 :: IO ()
game2048 = do
    let initialBoard = addRandomTile $ addRandomTile (replicate 4 (replicate 4 0))
    gameLoop2048 initialBoard

gameLoop2048 :: Board -> IO ()
gameLoop2048 board = do
    clearScreen
    printBoard board
    if isGameOver board
        then do
            putStrLn "Game Over! No more moves possible."
            askPlayAgain game2048
        else do
            putStr "Press w/a/s/d to move or 'q' to quit: "
            move <- getChar
            _ <- getLine
            case move of
                'w' -> gameLoop2048 (addRandomTile $ moveUp board)
                'a' -> gameLoop2048 (addRandomTile $ moveLeft board)
                's' -> gameLoop2048 (addRandomTile $ moveDown board)
                'd' -> gameLoop2048 (addRandomTile $ moveRight board)
                'q' -> putStrLn "Exiting the game..." >> exitSuccess
                _   -> gameLoop2048 board


askPlayAgain :: IO () -> IO ()
askPlayAgain game = do
  putStrLn "Do you want to play again? (y/n)"
  choice <- getLine
  case choice of
    "y" -> game
    "n" -> mainMenu
    _   -> askPlayAgain game

printBoard :: Board -> IO ()
printBoard board = do
    putStrLn $ unlines $ map (unwords . map showTile) board
  where
    showTile 0 = "."
    showTile n = show n

moveLeft :: Board -> Board
moveLeft = map mergeRow

moveRight :: Board -> Board
moveRight = map (reverse . mergeRow . reverse)

moveUp :: Board -> Board
moveUp = transpose . moveLeft . transpose

moveDown :: Board -> Board
moveDown = transpose . moveRight . transpose

mergeRow :: [Int] -> [Int]
mergeRow row = let nonZero = filter (/= 0) row
                   merged = combine nonZero
               in merged ++ replicate (length row - length merged) 0

combine :: [Int] -> [Int]
combine (x:y:xs)
    | x == y    = x * 2 : combine xs
    | otherwise = x : combine (y:xs)
combine xs = xs

addRandomTile :: Board -> Board
addRandomTile board = do
    let emptyTiles = [(r, c) | r <- [0..3], c <- [0..3], board !! r !! c == 0]
    if null emptyTiles
        then board
        else let (r, c) = head emptyTiles
                 newTile = if odd (length emptyTiles) then 4 else 2
             in updateBoard board r c newTile

updateBoard :: Board -> Int -> Int -> Int -> Board
updateBoard board r c val =
    take r board ++
    [take c (board !! r) ++ [val] ++ drop (c + 1) (board !! r)] ++
    drop (r + 1) board

isGameOver :: Board -> Bool
isGameOver board = null [ () | r <- [0..3], c <- [0..3], canMove r c ]
  where
    canMove r c = board !! r !! c == 0 ||
                  any (== board !! r !! c) (neighbors r c)
    neighbors r c = [board !! r' !! c' |
                     (r', c') <- [(r-1,c), (r+1,c), (r,c-1), (r,c+1)],
                     r' >= 0, r' <= 3, c' >= 0, c' <= 3]

askReplay :: IO () -> IO ()
askReplay game = do
  putStrLn "Do you want to play again? (y/n)"
  choice <- getLine
  case choice of
    "y" -> game
    "n" -> mainMenu
    _   -> askReplay game

-- Main
mainMenu :: IO ()
mainMenu = mainMenuLoop ["Hangman", "2048", "Quit"] 0

mainMenuLoop :: [String] -> Int -> IO ()
mainMenuLoop options selection = do
  clearScreen
  putStrLn "Use 'w'/'s' to move up/down, Enter to select, or 'q' to quit."
  mapM_ (putStrLn . formatOption selection) (zip [0..] options)
  
  direction <- getChar
  _ <- getLine

  case direction of
    'w' -> mainMenuLoop options ((selection - 1 + length options) `mod` length options)
    's' -> mainMenuLoop options ((selection + 1) `mod` length options)
    '\n' -> case selection of
      0 -> hangman
      1 -> game2048
      2 -> putStrLn "Thanks for playing! Goodbye." >> exitSuccess
      _ -> mainMenu
    'q' -> exitSuccess 
    _ -> mainMenuLoop options selection

formatOption :: Int -> (Int, String) -> String
formatOption selection (index, option) = if index == selection then "> " ++ option else "  " ++ option

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  mainMenu
