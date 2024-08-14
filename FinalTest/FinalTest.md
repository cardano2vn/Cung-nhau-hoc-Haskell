# Tiny Games


### Hangman: 
Đoán một từ bí mật bằng cách đề xuất các chữ cái. Mỗi lần đoán sai, một phần của hình người treo cổ sẽ được vẽ. Trò chơi kết thúc khi người chơi đoán đúng từ hoặc hình người treo cổ hoàn thành. 
* Từ bí mật được đọc ra và chọn ngẫu nhiên từ file
* Hiển thị trạng thái hiện tại của từ cần đoán, số lần đoán còn lại, các chữ cái đã đoán
* Trong khi chơi, nếu người dùng nhấn `ESC` thì thoát trò chơi, quay lại Main Menu
* Kết thúc trò chơi khi người chơi đoán đúng từ hoặc hết số lần đoán
* Sau khi kết thúc trò chơi, cho phép người chơi chọn chơi lại hoặc quay lại Main Menu

Hiển thị của game có thể như sau:
```
    ___
   |  \|
       |
       |
       |
       |
     __|__

Word: ------

 a   b   c   d   e   f   g   h   i  
 j   k   l   m   n   o   p   q   r  
 s   t   u   v   w   x   y   z  

Remaining guesses: 6
Enter your guess. 
```
```
v       Wrong guess!
    ___
   |  \|
   o   |
  -+   |
       |
       |
     __|__

Word: w--d--

 a   b   c  [d]  e  [f]  g   h   i  
 j   k   l   m   n   o   p   q   r  
 s   t   u  [v] [w] [x]  y   z  

Remaining guesses: 3
Enter your guess.
```
```
x       You've already guessed that letter.
    ___
   |  \|
   o   |
  -+   |
       |
       |
     __|__

Word: w--d--

 a   b   c  [d]  e  [f]  g   h   i  
 j   k   l   m   n   o   p   q   r  
 s   t   u  [v] [w] [x]  y   z  

Remaining guesses: 3
Enter your guess.
```
```
@       Please enter an alphabet letter.
    ___
   |  \|
   o   |
  -+   |
       |
       |
     __|__

Word: w--d--

 a   b   c  [d]  e  [f]  g   h   i  
 j   k   l   m   n   o   p   q   r  
 s   t   u  [v] [w] [x]  y   z  

Remaining guesses: 3
Enter your guess.
```
```
r       Good guess!
    ___
   |  \|
   o   |
  -+-  |
       |
       |
     __|__

Word: wonder

 a   b   c  [d] [e] [f]  g   h   i  
 j   k  [l]  m  [n] [o]  p   q  [r] 
 s   t   u  [v] [w] [x]  y   z  

Remaining guesses: 2
Congratulations! You've guessed the word!
Do you want to play again? (y/n): 
```
```
w       Wrong guess!
    ___
   |  \|
   o   |
  -+-  |
  / \  |
       |
     __|__

Word: ---c-

[a]  b  [c] [d]  e   f   g   h   i  
[j] [k]  l   m   n   o   p   q   r  
 s   t   u  [v] [w]  x   y   z  

Remaining guesses: 0
Game over! You've been hanged! The word was: fence
Do you want to play again? (y/n):
```

### 2048 - trò chơi ghép số trên bảng 4x4
Người chơi di chuyển các ô số theo 4 hướng (lên, xuống, trái, phải). Khi hai ô có cùng giá trị chạm vào nhau, chúng sẽ kết hợp thành một ô có giá trị gấp đôi. Mục tiêu là tạo ra ô có giá trị 2048.
* Khởi tạo trò chơi, bảng có 2 ô ngãu nhiên có giá trị 2 hoặc 4
* Kết hợp các cặp ô có cùng giá trị khi chúng va chạm theo hướng di chuyển
* Sau mỗi lượt di chuyển hợp lệ, thêm ngẫu nhiên một số mới (2 hoặc 4) vào 1 ô ngẫu nhiên đang trống trong bảng. Xác suất gặp số 2 là 75%, 4 là 25%
* Trong khi chơi, nếu người dùng nhấn `ESC` thì thoát trò chơi, quay lại Main Menu
* Kết thúc trò chơi khi không thể di chuyển theo bất kì hướng nào hoặc đạt được ô 2048
* Sau khi kết thúc trò chơi, cho phép người chơi chọn chơi lại hoặc quay lại Main Menu

Hiển thị của game có thể như sau:
```
     .     .     .     .
     .     2     .     2
     .     .     .     .
     .     .     .     .

Press w/a/s/d to move.
```

```

     2     4    16     2
     4     2     4    16
     2     4    16     8
     4    32     4     2

Game Over! No more moves possible.
Do you want to play again? (y/n): s
Invalid input. Please enter 'y' or 'n'.
```


### Main Menu
* Hiển thị menu chính cho phép người dùng chọn trò chơi muốn chơi hoặc thoát ứng dụng
* Thay vì lựa chọn các option bằng số, hãy thực hiện chức năng lựa chọn bằng phím di chuyển lên/xuống và nhấn Enter

```
Use 'w'/'s' to move up/down, and Enter to select.

> Hangman
  2048
  Quit
```
```
Use 'w'/'s' to move up/down, and Enter to select.

  Hangman
  2048
> Quit

Thanks for playing! Goodbye.
```