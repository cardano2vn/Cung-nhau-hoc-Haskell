module Game2048 where

import System.Random (randomRIO)
import Data.Char (ord)
globalBoardSize = 4
emptyBoard = [
        [0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 0, 0, 0]
    ]

type Line = [Int]
type Matrix = [Line]

-- move left
moveBackend :: Line -> Line
moveBackend [] = []
moveBackend [x] = [x]
moveBackend (0:rest) = moveBackend rest
moveBackend (x:y:rest) | x == y = (x * 2) : moveBackend rest
moveBackend (x:y:rest) = x : moveBackend (y:rest)

refill :: Int -> Line -> Line
refill expectedSize list | length list < expectedSize = refill expectedSize (list ++ [0])
refill _ list = list

moveLineLeft :: Line -> Line
moveLineLeft list = refill globalBoardSize $ moveBackend $ filter (/= 0) list

moveLeft :: Matrix -> Matrix
moveLeft = map moveLineLeft

moveRight :: Matrix -> Matrix
moveRight = map (reverse . moveLineLeft . reverse)

moveUp :: Matrix -> Matrix
moveUp = transpose . moveLeft . transpose

moveDown :: Matrix -> Matrix
moveDown = transpose . moveRight . transpose

-- temporary: check 32 for faster win :)
checkWin :: Matrix -> Bool
checkWin = foldr (\ line -> (||) (32 `elem` line)) False

checkFull :: Matrix -> Bool
checkFull m = not (foldr (\ line -> (||) (0 `elem` line)) False m)

isStuck :: Matrix -> Bool
isStuck m = (moveLeft m == m) &&
            (moveRight m == m) &&
            (moveUp m == m) &&
            (moveDown m == m)

insertItem :: Matrix -> (Int, Int) -> Int -> Maybe Matrix
insertItem m (row, col) _ | row >= length m || col >= length m = Nothing
insertItem m (row, col) _ | (m !! row !! col) /= 0 = Nothing
insertItem m (row, col) v =
    let newLine = replaceAt col v $ m !! row
    in Just $ replaceAt row newLine m

randomInsert :: Matrix -> IO Matrix
randomInsert m = do
    if checkFull m then return m
    else do
        row <- randomRIO (0, length m - 1) 
        col <- randomRIO (0, length m - 1)
        twoOrFour <- randomRIO (0 :: Int, 3 :: Int)
        -- 25% to get 4, 75% to get 2
        let matrix = insertItem m (row, col) (if twoOrFour == 0 then 4 else 2)
        case matrix of
            Nothing -> randomInsert m
            Just newMatrix -> return newMatrix

-- adds spaces to string to make it always has length of 5 -> pretty print
fillString :: String -> String
fillString str | length str < 5 = fillString $ " " ++ str
fillString str = str

printLine :: Line -> String
printLine = foldr (\x -> (++) (if x == 0 then "    ." else fillString $ show x)) ""

printMatrix :: Matrix -> String
printMatrix [] = ""
printMatrix (x:xs) = printLine x ++ "\n" ++ printMatrix xs

play2048 :: IO ()
play2048 = do
    board1 <- randomInsert emptyBoard
    board2 <- randomInsert board1
    game2048 board2

game2048 :: Matrix -> IO()
game2048 m = do
    putStrLn $ printMatrix m
    if checkWin m then 
        putStrLn "You win"
    else if isStuck m then do 
        putStrLn "You lose. No more move can be made"
        putStrLn $ printMatrix m
    else do
        putStrLn "Press w/a/s/d to move."
        chr <- getChar
        putStrLn ""
        if chr == toEnum 27 then
            putStrLn "Exit game 2048"
        else if chr `notElem` "wasd" then do
            putStrLn "Invalid move"
            game2048 m
        else do
            let newMatrix = case chr of 
                    'a' -> moveLeft m
                    'd' -> moveRight m
                    'w' -> moveUp m
                    's' -> moveDown m
                    _ -> m
            newMatrix2 <- randomInsert newMatrix
            game2048 newMatrix2

-- helper functions
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose list = map head list : transpose (map tail list)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 v (_:xs) = v : xs
replaceAt n v (x:xs) = x : replaceAt (n - 1) v xs