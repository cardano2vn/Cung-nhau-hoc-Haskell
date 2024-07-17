import Data.List(foldl')

{- Question 1 -}
-- Xuất phát từ binary search tree với kiểu Tree trong bài học
data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

threeLevelTree :: Tree Integer
threeLevelTree = Node 8
                    (Node 3
                      (Node 1 Empty Empty)
                      (Node 6 Empty Empty)
                    )
                    (Node 10
                      Empty
                      (Node 14 Empty Empty)
                    )
-- 1a) Viết hàm `insert` để thêm một phần tử vào một binary search tree có sẵn. 
--     Nếu phần tử đã có trong cây rồi thì không thêm.
{- ví dụ
ghci> insert threeLevelTree 0
Node 8 (Node 3 (Node 1 (Node 0 Empty Empty) Empty) (Node 6 Empty Empty)) (Node 10 Empty (Node 14 Empty Empty))

ghci> insert threeLevelTree 10
Node 8 (Node 3 (Node 1 Empty Empty) (Node 6 Empty Empty)) (Node 10 Empty (Node 14 Empty Empty))
-}
insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty x = Node x Empty Empty
insert tree@(Node y left right) x
  | x < y    = Node y (insert left x) right
  | x > y    = Node y left (insert right x)
  | otherwise = tree

{- 1b) Từ hàm trên, viết hàm `insertList` để  thêm 1 list các phần tử vào 1 cây binary search tree 

ghci> insertList threeLevelTree [1,2,5,20]
Node 8 (Node 3 (Node 1 Empty (Node 2 Empty Empty)) (Node 6 (Node 5 Empty Empty) Empty)) (Node 10 Empty (Node 14 Empty (Node 20 Empty Empty)))

ghci> insertList Empty [1,2,1,5,5,20]
Node 1 Empty (Node 2 Empty (Node 5 Empty (Node 20 Empty Empty)))
-}

insertList :: (Ord a) => Tree a -> [a] -> Tree a
insertList = foldl' insert

{-  1c) Tận dụng những thứ đã tạo ở trên để viết hàm `treeSort` với ý tưởng sau:
        - Nhận vào một danh sách các phần tử.
        - Chuyển tất cả các phần tử trong danh sách vào một cây nhị phân.
        - Chuyển lại cây nhị phân thành một danh sách với các phần tử được sắp xếp theo thứ tự tăng dần. 
    (Với hàm này, các phần tử trùng lặp cũng sẽ được loại bỏ khỏi danh sách kết quả)

ví dụ
  ghci> treeSort  [1,3,10,100,3,25,1]
  [1,3,10,25,100]

  ghci> treeSort  ['r','e','1','b','a','b','e']
  "1aber"

  ghci> treeSort "cardano2vn"
  "2acdnorv"
-}
treeToAscendList :: Tree a -> [a]
treeToAscendList Empty = []
treeToAscendList (Node x left right) = treeToAscendList left ++ 
                                       x : treeToAscendList right

treeSort :: (Ord a) => [a] -> [a]
treeSort = treeToAscendList . insertList Empty



{- Question 2 
Viết một hàm nhận vào một Binary Search Tree và hai giá trị của hai nút A và B, 
kiểm tra xem có tồn tại đường đi từ A đến B không. 
Lưu ý: chúng ta chỉ có thể đi theo một chiều từ nút cha đến nút con. Theo hình minh họa nó là chiều từ trên xuống.

          10
        /    \
       5      15
      / \    /  \
     3   7  13  20
    / \   \       \
   1   4   8      24

ghci> hasPath testTree 15 24
True
ghci> hasPath testTree 10 3
True
ghci> hasPath testTree 10 25
False
ghci> hasPath testTree 15 5
False
ghci> hasPath (insertList Empty "haskell-club") 'a' 'c'
True
-}
testTree :: Tree Integer
testTree = insertList Empty [10,5,15,3,7,13,20,1,4,8,24] 

hasPath :: Ord a => Tree a -> a -> a -> Bool
hasPath tree start end = start `elem` pathFromRoot end tree

pathFromRoot :: Ord a => a -> Tree a -> [a]
pathFromRoot _ Empty = []
pathFromRoot x (Node a left right)
    | x == a = [a]
    | x < a = let path = pathFromRoot x left
              in if null path then [] else a : path
    | x > a = let path = pathFromRoot x right
              in if null path then [] else a : path



{- Question 3 -}
-- Chúng ta có kiểu `Expr` biểu diễn các biểu thức số học với các phép toán Cộng, Trừ, Nhân, Chia và các Hằng số nguyên.

data Expr = Const Integer 
          | Add Expr Expr 
          | Sub Expr Expr 
          | Mul Expr Expr 
          | Div Expr Expr
          deriving (Show)

-- Ví dụ: 
expr1 :: Expr
expr1 = Add (Const 5) 
            (Mul (Const 2) 
                 (Sub (Const 3) 
                      (Const 10)))
-- biểu thị biểu thức: 5 + (2 * (3 - 10))
expr2 :: Expr
expr2 = Mul (Add (Const 5)
                 (Const 10)) 
            (Div (Const 36) 
                 (Add (Const (-1)) 
                      (Const 7)))
-- (5 + 10) * ( 36 `div` ( (-1) + 7 ) )

{- 3a  Viết hàm tính giá trị của biểu thức được khai báo bằng kiểu trên
ghci> evalExpr expr1
-9
ghci> evalExpr expr2
90
-}
evalExpr :: Expr -> Integer
evalExpr (Const n) = n
evalExpr (Add x y) = evalExpr x   +   evalExpr y
evalExpr (Sub x y) = evalExpr x   -   evalExpr y
evalExpr (Mul x y) = evalExpr x   *   evalExpr y
evalExpr (Div x y) = evalExpr x `div` evalExpr y

{- 3b Chúng ta nhận thấy hàm trên sẽ gặp lỗi với trường hợp chia cho 0: 
    ghci> evalExpr $ Div (Const 1) (Const 0)
    *** Exception: divide by zero

Trong những tình huống như này chúng ta có thể dùng kiểu `Maybe` để xử lý.
Định nghĩa của nó giống như kiểu `Box` trong bài học:
    data Maybe a = Nothing | Just a
Chính xác hơn thì `Maybe` là 1 type contructor.
Giá trị `Maybe a` thể hiện khả năng một biểu thức có thể trả về hoặc không trả về một giá trị hợp lệ (optional values)

Hãy dùng `Maybe` đê viết lại hàm `evalExpr` trên, để  có thể sử dụng nó an toàn cho cả trường hợp chia cho 0

  ghci> evalExprMaybe  $ Div (Const 200) (Const 0)
  Nothing
  ghci> evalExprMaybe  $ Div (Const 200) (Const 10)
  Just 20
  ghci> evalExprMaybe expr1
  Just (-9)
  ghci> evalExprMaybe expr2
  Just 90
--}

evalExprMaybe :: Expr -> Maybe Integer
evalExprMaybe (Const n)  =  Just n
evalExprMaybe (Add x y)  =
    case (evalExprMaybe x, evalExprMaybe y) of
        (Just a, Just b) -> Just (a + b)
        _                -> Nothing
evalExprMaybe (Sub x y)  =
    case (evalExprMaybe x, evalExprMaybe y) of
        (Just a, Just b) -> Just (a - b)
        _                -> Nothing
evalExprMaybe (Mul x y)  =
    case (evalExprMaybe x, evalExprMaybe y) of
        (Just a, Just b) -> Just (a * b)
        _                -> Nothing
evalExprMaybe (Div x y)  =
    case (evalExprMaybe x, evalExprMaybe y) of
        (_     , Just 0) -> Nothing
        (Just a, Just b) -> Just (a `div` b)
        _                -> Nothing



{- Question 4: 
Viết một hàm kiểm tra xem nước đi của 1 quân cờ vua trên bàn cờ trống có hợp lệ hay không.
Hàm nhận vào loại quân cờ, vị trí hiện tại của nó trên bàn cờ, và vị trí mới (vị trí được biểu thị bằng tọa độ hàng và cột).
Rồi trả về  True nếu nước đi đến vị trí mới hợp lệ, False nếu không hợp lệ.

Để đơn giản, chúng ta chỉ xét đến 3 loại quân sau:
  Quân Vua (King)
  Quân Xe (Rook)
  Quân Mã (Knight)

Yêu cầu:
  Định nghĩa các kiểu dữ liệu cần thiết:
    Kiểu dữ liệu đại diện cho các loại quân cờ: `PieceType`
    Kiểu dữ liệu đại diện cho vị trí trên bàn cờ (hàng, cột): `Position`
      
  Viết hàm isValidMove với chữ ký như sau:
    isValidMove :: PieceType -> Position -> Position -> Bool
  Ngoài ra, trước tiên, hàm cũng cần check các tọa độ truyền vào có hợp lệ hay không, với hàng và cột là các số nguyên từ 0 -> 7

Ví dụ:
ghci> isValidMove Rook (0, 0) (0, 5)
True
ghci> isValidMove Rook (2, 5) (0, 5)
True
ghci> isValidMove Rook (1, 0) (1, 8)
False

ghci> isValidMove King (4, 4) (5, 4)
True
ghci> isValidMove King (4, 4) (5, 5)
True
ghci> isValidMove King (4, 4) (5, 6)
False

ghci> isValidMove Knight (7, 1) (5, 2)
True
ghci> isValidMove Knight (7, 1) (8, 3)
False
ghci> isValidMove Knight (7, 1) (6, 3)
True
-}
data PieceType = King | Rook | Knight
type Position = (Int, Int)

isValidMove :: PieceType -> Position -> Position -> Bool
isValidMove _ pos1 pos2 | not (isValidPos pos1 && isValidPos pos2) = False
isValidMove King   (x1, y1) (x2, y2) = abs (x2 - x1) <= 1 && abs (y2 - y1) <= 1
isValidMove Knight (x1, y1) (x2, y2) = (abs (x2 - x1), abs (y2 - y1)) `elem` [(1, 2), (2, 1)]
isValidMove Rook   (x1, y1) (x2, y2) = x1 == x2 || y1 == y2

isValidPos :: Position -> Bool
isValidPos (x, y) = x `elem` [0..7] && 
                    y `elem` [0..7]