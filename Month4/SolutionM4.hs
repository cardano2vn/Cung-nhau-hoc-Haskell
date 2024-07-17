-- 1 số hàm gợi ý, các bạn có thể cân nhắc sử dụng 
import Data.Char (isDigit)
import Text.Read ( readMaybe )
import System.Directory (createDirectory, renameFile, removeFile, 
                         removeDirectory, listDirectory, doesFileExist)

{- Question 1
Khi tạo 1 NFT theo CIP-25, chúng ta cần đính kèm metadata vào giao dịch.
Một metadata đơn giản chứa 1 số thông tin sau:

{
    "721": {
        "<policy_ID>": {
            "<asset_name>": {
                "description": "<desc>",
                "name": "<asset_name>",  -- Tên hiển thị của NFT, trên thực tế, không cần giống <asset_name>
                "image": "img url"
            }
        }
    }
}

Viết hàm yêu cầu người dùng nhập vào 1 số thông tin: <policy_ID>, <asset_name>, <desc>
Từ đó tạo và ghi metadata vào file metadata.json

-}
metadataFile :: FilePath
metadataFile = "metadata.json"

createMetadata :: FilePath -> IO ()
createMetadata file = do
    putStr "Enter PolicyID: "
    policyId <- getLine

    putStr "Enter Asset name: "
    assetName <- getLine

    putStr "Enter Description: "
    desc <- getLine

    let metadata = unlines [
            "{",
            "    \"721\": {",
            "        \"" ++ policyId ++ "\": {",
            "            \"" ++ assetName ++ "\": {",
            "                \"description\": \"" ++ desc ++ "\",",
            "                \"name\": \"" ++ assetName ++"\",",
            "                \"image\": \"img url\"",
            "            }",
            "        }",
            "    }",
            "}"
          ]

    writeFile file metadata


{- Q2
Viết hàm thực hiện các nhiệm vụ sau:
    - Tạo 1 thư mục con trong thư mục hiện tại. 
    - Di chuyển file tạo ở bài trên vào thư mục con
    - Đổi tên file
    - Xóa file
    - Xóa thư mục con
Sau mỗi thao tác trên, in ra cây thư mục bằng hàm đã tạo trong homework week11, để xem kết quả
-}

someDirectoryFunctions :: FilePath -> IO()
someDirectoryFunctions file = do
    
    
    -- Tạo 1 thư mục con trong thư mục hiện tại.
    let subDir = "SubDir"
    createDirectory subDir
    printDirStruct

    -- Di chuyển file tạo ở bài trên vào thư mục con
    let newFile = subDir ++ "/" ++ file
    renameFile file newFile
    printDirStruct

    -- Đổi tên file
    let renamedFile = subDir ++ "/renamed.json"
    renameFile newFile renamedFile
    printDirStruct

    -- Xóa file
    removeFile renamedFile
    printDirStruct

    -- Xóa thư mục
    removeDirectory subDir
    printDirStruct


printDirStruct :: IO ()
printDirStruct = putStrLn "." 
                 >> printItemsSameLevel 0 "."
  where 
    printItemsSameLevel :: Int -> FilePath -> IO ()
    printItemsSameLevel level path = do
      filesAndFolders <- listDirectory path
      
      if null filesAndFolders
        then return ()
        else do
          let firstItems = init filesAndFolders
              lastItem = last filesAndFolders
              
              preSpace = replicate (4 * level) ' '
              
              normalBranch = "├── "
              endBranch    = "└── "

              printItemAndInside :: String -> String -> IO()
              printItemAndInside branchLine itemName = do
                putStrLn $ preSpace ++ branchLine ++ itemName -- print item itself
                -- check if this item is a file or folder
                let fPath = path ++ "/" ++ itemName
                isFile <- doesFileExist fPath
                if isFile 
                    then return ()  -- item is a file
                    else printItemsSameLevel (level + 1) fPath -- item is a folder, recruise for all sub-items inside
          
          -- print structure for first items
          mapM_ (printItemAndInside normalBranch) firstItems
          -- print structure for last item
          printItemAndInside endBranch lastItem


{- Question 3
Khi dùng cardano-cli để query các UTxO trên 1 địa chỉ, chúng ta nhận được kết quả dạng như sau:

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1f845ad67df909fcea522c7ff0d3c73ff17ada2436c1d65314acfd532e9789f0     0        9826876 lovelace + TxOutDatumNone
ac6f50aeb4427bc1f381163f0fec6f67d7542150a96904a54d274fd685b90f42     0        33000000 lovelace + TxOutDatumNone
ac6f50aeb4427bc1f381163f0fec6f67d7542150a96904a54d274fd685b90f42     1        66177669 lovelace + TxOutDatumNone
d88ba55e07f9c8a091b87f2550aa97b331f05c0618bbbf8b43cf186ea38de3b8     0        4829226 lovelace + TxOutDatumNone
dd30de76b03db635eae4de221678a5190866f32f692598c87cefb8401e8ae7fb     0        4829226 lovelace + TxOutDatumNone

Các thông tin này đang được lưu trong file ./utxo.txt

Hãy viết hàm `selectUtxo` thực hiện các việc sau:
- Đọc nội dung file utxo.txt, bỏ 2 dòng đầu, in ra các dòng còn lại kèm theo số thứ tự của chúng tính từ 1. 
- Yêu cầu người dùng lựa chọn 1 utxo theo số thứ tự. 
    Kiểm tra tính hợp lệ của số nhập vào (nằm trong khoảng hợp lệ dựa trên số lượng utxo)
- In ra thông tin của utxo được chọn bao gồm: mã tham chiếu (có dạng <TxHash>#<TxIx>) và số lovelace chứa trong nó

Để rõ hơn, Hãy xem VD:

ghci> selectUtxo utxoFile 
Reading file ./utxo.txt
Utxo list:
1: 1f845ad67df909fcea522c7ff0d3c73ff17ada2436c1d65314acfd532e9789f0     0        9826876 lovelace + TxOutDatumNone
2: ac6f50aeb4427bc1f381163f0fec6f67d7542150a96904a54d274fd685b90f42     0        33000000 lovelace + TxOutDatumNone
3: ac6f50aeb4427bc1f381163f0fec6f67d7542150a96904a54d274fd685b90f42     1        66177669 lovelace + TxOutDatumNone
4: d88ba55e07f9c8a091b87f2550aa97b331f05c0618bbbf8b43cf186ea38de3b8     0        4829226 lovelace + TxOutDatumNone
5: dd30de76b03db635eae4de221678a5190866f32f692598c87cefb8401e8ae7fb     0        4829226 lovelace + TxOutDatumNone
Please enter the line number of utxo you want to select:
aa
Invalid input. Please enter a number:
123k
Invalid input. Please enter a number:
0
Invalid number. Please enter a line number in valid range:
10
Invalid number. Please enter a line number in valid range:
2
You select utxo ac6f50aeb4427bc1f381163f0fec6f67d7542150a96904a54d274fd685b90f42#0 with 33000000 lovelace.

-}
utxoFile :: FilePath
utxoFile = "./utxo.txt"

-- Read utxo file, skip the first two lines, and print the utxo list with line numbers
printUtxosFromFile :: FilePath -> IO [String]
printUtxosFromFile filePath = do
    contents <- readFile filePath
    let linesOfFile = drop 2 (lines contents)  -- Skip the first two lines
    mapM_ printLineWithNumber (zip [1..] linesOfFile)
    return linesOfFile
  where
    -- Print a line with its number
    printLineWithNumber :: (Int, String) -> IO ()
    printLineWithNumber (num, line) = putStrLn $ show num ++ ": " ++ line

-- Get user selection, validate it
getUserSelection :: [String] -> IO String
getUserSelection utxoLines = do
    input <- getLine
    let maybeLineNumber = readMaybe input :: Maybe Int
    case maybeLineNumber of
        Nothing -> do
            putStrLn "Invalid input. Please enter a number:"
            getUserSelection utxoLines
        Just n  ->
            if n > 0 && n <= length utxoLines then
                return (utxoLines !! (n - 1))
            else do
                putStrLn "Invalid number. Please enter a line number in valid range:"
                getUserSelection utxoLines


-- print the selected line with formatted output
printSelectedUtxo :: String -> IO ()
printSelectedUtxo line = do
    case words line of
        (txHash:txIx:amount:_) ->
            putStrLn $ "You select utxo " ++ txHash ++ "#" ++ txIx ++ " with " ++ amount ++ " lovelace."
        _                      -> 
            putStrLn "Invalid line format."

selectUtxo :: FilePath -> IO ()
selectUtxo filePath = do
    putStrLn $ "Reading file " ++ filePath
    putStrLn "Utxo list:"
    utxos <- printUtxosFromFile filePath
    putStrLn "Please enter the line number of utxo you want to select:"
    selectedUtxo <- getUserSelection utxos
    printSelectedUtxo selectedUtxo


{- Q4:
Viết hàm sumNumbers yêu cầu người dùng nhập vào một danh sách các số nguyên,
(mỗi số trên 1 dòng, bỏ qua các dòng không phải là số nguyên, kết thúc nhập khi nhận được 1 dòng trống) 
và sau đó in ra tổng của các số đó. Lưu ý: hàm cần chấp nhận cả số âm

Ví dụ:
ghci> sumNumbers 
Enter numbers (one per line), and an empty line to end:
1
2
20

The sum is: 23


ghci> sumNumbers 
Enter numbers (one per line), and an empty line to end:
-10
3a
abcd
--30
110

The sum is: 100

-}

sumNumbers :: IO ()
sumNumbers = do
    putStrLn "Enter numbers (one per line), and an empty line to end:"
    numbers <- readNumbers
    putStrLn $ "The sum is: " ++ show (sum numbers)
  where
    readNumbers :: IO [Integer]
    readNumbers = do
        line <- getLine

        if null line then return []
        else case readMaybe line :: Maybe Integer of
            Nothing -> readNumbers
            Just n  -> do
                rest <- readNumbers
                return (n : rest)
            

