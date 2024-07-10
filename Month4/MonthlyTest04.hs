-- 1 số hàm gợi ý, các bạn có thể cân nhắc sử dụng 
import Data.Char (isDigit)
import Text.Read ( readMaybe )


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



{- Q2
Viết hàm thực hiện các nhiệm vụ sau:
    - Tạo 1 thư mục con trong thư mục hiện tại. 
    - Di chuyển file tạo ở bài trên vào thư mục con
    - Đổi tên file
    - Xóa file
    - Xóa thư mục con
Sau mỗi thao tác trên, in ra cây thư mục bằng hàm đã tạo trong homework week11, để xem kết quả
-}


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