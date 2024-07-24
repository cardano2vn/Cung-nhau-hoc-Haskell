import Data.List (sort)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), fromRight)
import Control.Monad (mapM_)
import qualified Data.Text as T

-- 1. Data.List - sort
-- Hàm sort được sử dụng để sắp xếp một danh sách theo thứ tự tăng dần.

sortedList :: [Int]
sortedList = sort [3, 1, 2, 5, 4]

-- 2. Data.Maybe - Maybe
-- Kiểu dữ liệu Maybe được sử dụng để biểu diễn một giá trị có thể có hoặc không có. Nó giúp tránh các lỗi null reference bằng cách buộc lập trình viên phải xử lý cả hai trường hợp.

findElement :: Eq a => a -> [a] -> Maybe a
findElement _ [] = Nothing
findElement x (y:ys)
  | x == y    = Just y
  | otherwise = findElement x ys

result :: Maybe Int
result = findElement 3 [1, 2, 3, 4, 5]

value :: Int
value = fromMaybe 0 result

-- 3. Data.Either - Either
-- Kiểu dữ liệu Either được sử dụng để biểu diễn một giá trị có thể là một trong hai kiểu khác nhau. Thường được dùng trong các hàm có thể trả về lỗi.

divide :: Int -> Int -> Either String Int
divide _ 0 = Left "Division by zero"
divide x y = Right (x `div` y)

resultDivide :: Either String Int
resultDivide = divide 10 2

valueDivide :: Int
valueDivide = Data.Either.fromRight 0 resultDivide

-- 4. Control.Monad - mapM_
-- Hàm mapM_ được sử dụng để áp dụng một hành động monadic lên mỗi phần tử của danh sách, không trả về giá trị kết quả.

printNumbers :: [Int] -> IO ()
printNumbers nums = mapM_ print nums

-- 5. Data.Text từ thư viện text
-- Data.Text là một kiểu dữ liệu để làm việc với chuỗi ký tự hiệu quả hơn so với kiểu String truyền thống. Nó hỗ trợ nhiều thao tác chuỗi như nối, cắt, và tìm kiếm.

exampleText :: T.Text
exampleText = T.pack "Hello, Haskell!"

findSubstring :: T.Text -> T.Text -> Bool
findSubstring sub text = T.isInfixOf sub text

-- Test chương trình
mainTest :: IO ()
mainTest = do
    -- Test Data.List
    print sortedList  -- Output: [1, 2, 3, 4, 5]

    -- Test Data.Maybe
    print result       -- Output: Just 3
    print value        -- Output: 3

    -- Test Data.Either
    print resultDivide -- Output: Right 5
    print valueDivide  -- Output: 5

    -- Test Control.Monad
    printNumbers [1, 2, 3, 4, 5]

    -- Test Data.Text
    print (findSubstring (T.pack "Haskell") exampleText)  -- Output: True
    print (T.toUpper exampleText)                         -- Output: "HELLO, HASKELL!"


