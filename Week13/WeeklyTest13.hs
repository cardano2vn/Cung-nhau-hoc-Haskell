{-
Update cho ForestGame trong bài học để bổ sung thêm tính năng chiến đấu với quái vật.

- Golem có khả năng xuất hiện ngẫu nhiên sau mỗi lần di chuyển của Player hoặc ngay đầu trò chơi.
- Player và Golem có 2 chỉ số là các số nguyên: sức tấn công `Attack` và máu `HP` 
  Các chỉ số này đều được tạo ngâu nhiên từ 1 khoảng nhất định 
- Khi gặp Golem thì Player có 2 lựa chọn:
    + Fight: đánh nhau 1 hiệp với Golem. Sau hiệp đấu cả Player và Golem sẽ đều bị giảm HP.
      Giá trị HP giảm phụ thuộc vào sức tấn công của đối thủ và độ may mắn của đòn tấn công.
            HP giảm = Luck đối thủ * Attack đối thủ
      Luck là ngẫũ nhiên cho cả 2 bên.
    + RunAway: Player chọn chạy trốn, thì sẽ có 2 tình huống ngẫu nhiên xảy ra:
        * Chạy thoát -> Tiếp tục di chuyển trong rừng
        * Chạy không thoát: Player sẽ bị Golem đánh và mất máu HP. Lượng giảm được tính tương tự như trên
          HP của Golem vẫn giữ nguyên
  Lặp lại 2 lựa chọn trên cho đến khi 1 trong 2 bên hết HP. Nếu Player thắng -> tiếp tục đi trong rừng.
-}
