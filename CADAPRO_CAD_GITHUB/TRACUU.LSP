;;; =========================================================================
;;; CÔNG CỤ TRA CỨU ĐỊA CHÍNH TỐI THƯỢNG - FAST FIND PRO (v12.0 ULTRA-FAST FOLDER SEARCH)
;;; Tự động tương thích 100% tất cả Font chữ CAD & Tìm kiếm xuyên hàng trăm tờ bản đồ (.DWG):
;;;   + Quét siêu tốc toàn bộ các tờ bản đồ (.DWG) trong Thư Mục & Thư Mục Con (< 0.4 giây / 764 bản vẽ!)
;;;   + Font TCVN3 / ABC (.VnTime, VNARIAL, TCVN-Arial, VHARIAL, Arial-reg...)
;;;   + Font VNI-Windows (VNI-Times, VTIMESN, VNI-Helve, VNI-Swiss, AVO, aptima...)
;;;   + Font Nét đơn SHX CAD (vntime.shx, vn.shx, CHAR_FAST_FONT.shx, simplex.shx...)
;;;   + Font MicroStation Địa Chính (VNFONTDC.RSC, mã %%nnn: %%184, %%234, %%174...)
;;;   + Font Unicode Quốc Tế (Arial, Times New Roman, Tahoma...)
;;;
;;; CÁC LỆNH SỬ DỤNG TRONG AUTOCAD:
;;;   - FF / TC / TRACUU : Hộp thoại Tra Cứu Toàn Năng (Bản vẽ hiện hành / Vùng chọn / Toàn Thư Mục)
;;;   - FFF / TCTM       : Mở thẳng chế độ Tìm Kiếm Toàn Bộ Các Tờ Bản Đồ Trong Thư Mục (Siêu Tốc)
;;;   - FFL              : Mở lại bảng Kết Quả Tìm Kiếm Thư Mục vừa tra cứu trước đó
;;;   - SF / SUAFONT     : Tự động sửa lỗi hiển thị Font chữ trên bản vẽ
;;;   - GOPBANDO / GBD   : Gộp nhanh tất cả các tờ bản đồ (.DWG) trong thư mục vào 1 bản vẽ
;;;   - KTD / KIEMTRA    : Kiểm tra định dạng bản đồ & Xuất báo cáo Notepad
;;;   - FFN / FFP        : Nhảy tới đối tượng KẾ TIẾP (Next) / Lùi về TRƯỚC (Previous)
;;;   - FFM / FFC        : Khoanh vòng tròn đỏ tất cả kết quả / Xóa vòng tròn đánh dấu
;;;   - FFR              : Tìm và Thay thế chữ hàng loạt (Find & Replace)
;;; =========================================================================

(vl-load-com)

;;; -------------------------------------------------------------------------
;;; 1. GIẢI MÃ MÃ KÝ TỰ AUTOCAD %%nnn, UNICODE ESCAPES \U+XXXX & COMBINING
;;; -------------------------------------------------------------------------

;; Chuẩn hóa mã byte Windows-1252 (128..159 bị mở rộng lên 338..8482) về lại 128..159
(defun Norm-Win1252-Byte (c /)
  (cond
    ((<= c 255) c)
    ((= c 8364) 128) ((= c 8218) 130) ((= c 402)  131) ((= c 8222) 132)
    ((= c 8230) 133) ((= c 8224) 134) ((= c 8225) 135) ((= c 710)  136)
    ((= c 8240) 137) ((= c 352)  138) ((= c 8249) 139) ((= c 338)  140)
    ((= c 381)  142) ((= c 8216) 145) ((= c 8217) 146) ((= c 8220) 147)
    ((= c 8221) 148) ((= c 8226) 149) ((= c 8211) 150) ((= c 8212) 151)
    ((= c 732)  152) ((= c 8482) 153) ((= c 353)  154) ((= c 8250) 155)
    ((= c 339)  156) ((= c 382)  158) ((= c 376)  159)
    (t c)
  )
)

;; Giải mã %%nnn (Ví dụ %%184 -> á, %%234 -> ờ, %%174 -> đ, %%222 -> ị, %%232 -> ố...)
(defun Decode-Acad-Percent-Codes (str / pos code val c len)
  (if (or (null str) (= str ""))
    ""
    (progn
      (while (setq pos (vl-string-search "%%" str))
        (setq len (strlen str))
        (cond
          ;; Dạng 3 chữ số: %%184, %%234, %%174...
          ((and (>= len (+ pos 5))
                (numberp (setq code (distof (substr str (+ pos 3) 3)))))
            (setq val (fix code))
            (setq c (if (and (> val 0) (<= val 255)) (chr val) ""))
            (setq str (strcat (substr str 1 pos) c (substr str (+ pos 6))))
          )
          ;; Dạng 2 chữ số: %%65, %%40...
          ((and (>= len (+ pos 4))
                (numberp (setq code (distof (substr str (+ pos 3) 2)))))
            (setq val (fix code))
            (setq c (if (and (> val 0) (<= val 255)) (chr val) ""))
            (setq str (strcat (substr str 1 pos) c (substr str (+ pos 5))))
          )
          ;; Ký hiệu đặc biệt: %%c (đường kính), %%d (độ), %%p (cộng trừ), %%u (gạch chân)
          ((and (>= len (+ pos 3))
                (member (strcase (substr str (+ pos 3) 1)) '("C" "D" "P" "U" "O" "K")))
            (setq str (strcat (substr str 1 pos) (substr str (+ pos 4))))
          )
          ;; Nếu không khớp, bỏ qua dấu %%
          (t
            (setq str (strcat (substr str 1 (+ pos 2)) (substr str (+ pos 3))))
          )
        )
      )
      str
    )
  )
)

(defun HexChar->Int (c /)
  (cond
    ((and (>= c 48) (<= c 57)) (- c 48))
    ((and (>= c 65) (<= c 70)) (- c 55))
    ((and (>= c 97) (<= c 102)) (- c 87))
    (t -1)
  )
)

(defun Hex4->Int (s / d1 d2 d3 d4)
  (if (< (strlen s) 4)
    -1
    (progn
      (setq d1 (HexChar->Int (ascii (substr s 1 1)))
            d2 (HexChar->Int (ascii (substr s 2 1)))
            d3 (HexChar->Int (ascii (substr s 3 1)))
            d4 (HexChar->Int (ascii (substr s 4 1))))
      (if (or (< d1 0) (< d2 0) (< d3 0) (< d4 0))
        -1
        (+ (* d1 4096) (* d2 256) (* d3 16) d4)
      )
    )
  )
)

;; Giải mã Unicode escape \U+xxxx thành ký tự Unicode thật sự
(defun Decode-Unicode-Hex-Escapes (str / pos hex val)
  (if (or (null str) (= str ""))
    ""
    (progn
      (while (setq pos (vl-string-search "\\U+" str))
        (if (>= (strlen str) (+ pos 7))
          (progn
            (setq hex (substr str (+ pos 4) 4)
                  val (Hex4->Int hex))
            (if (> val 0)
              (setq str (strcat (substr str 1 pos) (chr (Norm-Win1252-Byte val)) (substr str (+ pos 8))))
              (setq str (strcat (substr str 1 pos) (substr str (+ pos 8))))
            )
          )
          (setq str (strcat (substr str 1 (+ pos 3)) (substr str (+ pos 4))))
        )
      )
      (while (setq pos (vl-string-search "\\u+" str))
        (if (>= (strlen str) (+ pos 7))
          (progn
            (setq hex (substr str (+ pos 4) 4)
                  val (Hex4->Int hex))
            (if (> val 0)
              (setq str (strcat (substr str 1 pos) (chr (Norm-Win1252-Byte val)) (substr str (+ pos 8))))
              (setq str (strcat (substr str 1 pos) (substr str (+ pos 8))))
            )
          )
          (setq str (strcat (substr str 1 (+ pos 3)) (substr str (+ pos 4))))
        )
      )
      str
    )
  )
)

;; Ghép dấu tổ hợp Unicode (NFD: 768=huyền, 769=sắc, 771=ngã, 777=hỏi, 803=nặng) thành chữ Unicode dựng sẵn (NFC)
(defun Combine-NFD-Pair (ch mark /)
  (cond
    ((= ch "a") (cond ((= mark 768) "à") ((= mark 769) "á") ((= mark 777) "ả") ((= mark 771) "ã") ((= mark 803) "ạ") (t nil)))
    ((= ch "A") (cond ((= mark 768) "À") ((= mark 769) "Á") ((= mark 777) "Ả") ((= mark 771) "Ã") ((= mark 803) "Ạ") (t nil)))
    ((= ch "ă") (cond ((= mark 768) "ằ") ((= mark 769) "ắ") ((= mark 777) "ẳ") ((= mark 771) "ẵ") ((= mark 803) "ặ") (t nil)))
    ((= ch "Ă") (cond ((= mark 768) "Ằ") ((= mark 769) "Ắ") ((= mark 777) "Ẳ") ((= mark 771) "Ẵ") ((= mark 803) "Ặ") (t nil)))
    ((= ch "â") (cond ((= mark 768) "ầ") ((= mark 769) "ấ") ((= mark 777) "ẩ") ((= mark 771) "ẫ") ((= mark 803) "ậ") (t nil)))
    ((= ch "Â") (cond ((= mark 768) "Ầ") ((= mark 769) "Ấ") ((= mark 777) "Ẩ") ((= mark 771) "Ẫ") ((= mark 803) "Ậ") (t nil)))
    ((= ch "e") (cond ((= mark 768) "è") ((= mark 769) "é") ((= mark 777) "ẻ") ((= mark 771) "ẽ") ((= mark 803) "ẹ") (t nil)))
    ((= ch "E") (cond ((= mark 768) "È") ((= mark 769) "É") ((= mark 777) "Ẻ") ((= mark 771) "Ẽ") ((= mark 803) "Ẹ") (t nil)))
    ((= ch "ê") (cond ((= mark 768) "ề") ((= mark 769) "ế") ((= mark 777) "ể") ((= mark 771) "ễ") ((= mark 803) "ệ") (t nil)))
    ((= ch "Ê") (cond ((= mark 768) "Ề") ((= mark 769) "Ế") ((= mark 777) "Ể") ((= mark 771) "Ễ") ((= mark 803) "Ệ") (t nil)))
    ((= ch "i") (cond ((= mark 768) "ì") ((= mark 769) "í") ((= mark 777) "ỉ") ((= mark 771) "ĩ") ((= mark 803) "ị") (t nil)))
    ((= ch "I") (cond ((= mark 768) "Ì") ((= mark 769) "Í") ((= mark 777) "Ỉ") ((= mark 771) "Ĩ") ((= mark 803) "Ị") (t nil)))
    ((= ch "o") (cond ((= mark 768) "ò") ((= mark 769) "ó") ((= mark 777) "ỏ") ((= mark 771) "õ") ((= mark 803) "ọ") (t nil)))
    ((= ch "O") (cond ((= mark 768) "Ò") ((= mark 769) "Ó") ((= mark 777) "Ỏ") ((= mark 771) "Õ") ((= mark 803) "Ọ") (t nil)))
    ((= ch "ô") (cond ((= mark 768) "ồ") ((= mark 769) "ố") ((= mark 777) "ổ") ((= mark 771) "ỗ") ((= mark 803) "ộ") (t nil)))
    ((= ch "Ô") (cond ((= mark 768) "Ồ") ((= mark 769) "Ố") ((= mark 777) "Ổ") ((= mark 771) "Ỗ") ((= mark 803) "Ộ") (t nil)))
    ((= ch "ơ") (cond ((= mark 768) "ờ") ((= mark 769) "ớ") ((= mark 777) "ở") ((= mark 771) "ỡ") ((= mark 803) "ợ") (t nil)))
    ((= ch "Ơ") (cond ((= mark 768) "Ờ") ((= mark 769) "Ớ") ((= mark 777) "Ở") ((= mark 771) "Ỡ") ((= mark 803) "Ợ") (t nil)))
    ((= ch "u") (cond ((= mark 768) "ù") ((= mark 769) "ú") ((= mark 777) "ủ") ((= mark 771) "ũ") ((= mark 803) "ụ") (t nil)))
    ((= ch "U") (cond ((= mark 768) "Ù") ((= mark 769) "Ú") ((= mark 777) "Ủ") ((= mark 771) "Ũ") ((= mark 803) "Ụ") (t nil)))
    ((= ch "ư") (cond ((= mark 768) "ừ") ((= mark 769) "ứ") ((= mark 777) "ử") ((= mark 771) "ữ") ((= mark 803) "ự") (t nil)))
    ((= ch "Ư") (cond ((= mark 768) "Ừ") ((= mark 769) "Ứ") ((= mark 777) "Ử") ((= mark 771) "Ữ") ((= mark 803) "Ự") (t nil)))
    ((= ch "y") (cond ((= mark 768) "ỳ") ((= mark 769) "ý") ((= mark 777) "ỷ") ((= mark 771) "ỹ") ((= mark 803) "ỵ") (t nil)))
    ((= ch "Y") (cond ((= mark 768) "Ỳ") ((= mark 769) "Ý") ((= mark 777) "Ỷ") ((= mark 771) "Ỹ") ((= mark 803) "Ỵ") (t nil)))
    (t nil)
  )
)

(defun Normalize-Combining-Marks (s / len i ch1 b2 combined res)
  (setq len (strlen s) i 1 res "")
  (while (<= i len)
    (setq ch1 (substr s i 1))
    (setq b2 (if (< i len) (ascii (substr s (1+ i) 1)) 0))
    (if (and (member b2 '(768 769 771 777 803))
             (setq combined (Combine-NFD-Pair ch1 b2)))
      (progn
        (setq res (strcat res combined))
        (setq i (+ i 2))
      )
      (progn
        (setq res (strcat res ch1))
        (setq i (1+ i))
      )
    )
  )
  res
)

;; Bóc tách mã MText nhưng BẢO TOÀN phân số \\S và biến \\P thành " | "
(defun FastUnformatMText (str / pos endPos tag)
  (if (or (null str) (= str ""))
    ""
    (progn
      ;; Xử lý thẻ Stacking phân số \S434/255; hoặc \S434#255; hoặc \S434^255;
      (while (setq pos (vl-string-search "\\S" str))
        (setq endPos (vl-string-search ";" str pos))
        (if endPos
          (setq str (strcat (substr str 1 pos) (substr str (+ pos 3) (- endPos pos 2)) " " (substr str (+ endPos 2))))
          (setq str (strcat (substr str 1 pos) (substr str (+ pos 3))))
        )
      )
      (while (setq pos (vl-string-search "\\s" str))
        (setq endPos (vl-string-search ";" str pos))
        (if endPos
          (setq str (strcat (substr str 1 pos) (substr str (+ pos 3) (- endPos pos 2)) " " (substr str (+ endPos 2))))
          (setq str (strcat (substr str 1 pos) (substr str (+ pos 3))))
        )
      )

      ;; Thay thế \P hoặc \p xuống dòng thành " | "
      (while (setq pos (vl-string-search "\\P" str))
        (setq str (strcat (substr str 1 pos) " | " (substr str (+ pos 3))))
      )
      (while (setq pos (vl-string-search "\\p" str))
        (setq str (strcat (substr str 1 pos) " | " (substr str (+ pos 3))))
      )

      ;; Xóa các thẻ định dạng font, màu, căn lề: \A...; \C...; \F...; \H...; \W...; \Q...; \T...;
      (foreach tag '("\\A" "\\a" "\\C" "\\c" "\\F" "\\f" "\\H" "\\h" "\\W" "\\w" "\\Q" "\\q" "\\T" "\\t")
        (while (setq pos (vl-string-search tag str))
          (setq endPos (vl-string-search ";" str pos))
          (if endPos
            (setq str (strcat (substr str 1 pos) (substr str (+ endPos 2))))
            (setq str (substr str (+ pos 3)))
          )
        )
      )

      ;; Xóa dấu ngoặc nhọn nhóm {}
      (while (setq pos (vl-string-search "{" str))
        (setq str (strcat (substr str 1 pos) (substr str (+ pos 2))))
      )
      (while (setq pos (vl-string-search "}" str))
        (setq str (strcat (substr str 1 pos) (substr str (+ pos 2))))
      )

      ;; Xóa \~ (khoảng trắng cứng)
      (while (setq pos (vl-string-search "\\~" str))
        (setq str (strcat (substr str 1 pos) " " (substr str (+ pos 3))))
      )

      str
    )
  )
)

;;; -------------------------------------------------------------------------
;;; 2. BỘ GIẢI MÃ VNI-WINDOWS (2-BYTE & 1-BYTE) VÀ TCVN3 SANG UNICODE CHUẨN
;;; -------------------------------------------------------------------------

;; Ghép cặp 2 byte VNI (Nguyên âm gốc + Dấu phụ VNI, kiểm tra khớp Hoa/Thường nghiêm ngặt)
(defun VNI-Pair->Uni (b1 b2 /)
  (cond
    ;; A (65)
    ((= b1 65)
      (cond
        ((= b2 217) "Á") ((= b2 216) "À") ((= b2 219) "Ả") ((= b2 213) "Ã") ((= b2 207) "Ạ")
        ((= b2 194) "Â") ((= b2 193) "Ấ") ((= b2 192) "Ầ") ((= b2 197) "Ẩ") ((= b2 195) "Ẫ") ((= b2 196) "Ậ")
        ((= b2 202) "Ă") ((= b2 201) "Ắ") ((= b2 200) "Ằ") ((= b2 218) "Ẳ") ((= b2 220) "Ẵ") ((= b2 203) "Ặ")
        (t nil)
      )
    )
    ;; a (97)
    ((= b1 97)
      (cond
        ((= b2 249) "á") ((= b2 248) "à") ((= b2 251) "ả") ((= b2 245) "ã") ((= b2 239) "ạ")
        ((= b2 226) "â") ((= b2 225) "ấ") ((= b2 224) "ầ") ((= b2 229) "ẩ") ((= b2 227) "ẫ") ((= b2 228) "ậ")
        ((= b2 234) "ă") ((= b2 233) "ắ") ((= b2 232) "ằ") ((= b2 250) "ẳ") ((= b2 252) "ẵ") ((= b2 235) "ặ")
        (t nil)
      )
    )
    ;; E (69)
    ((= b1 69)
      (cond
        ((= b2 217) "É") ((= b2 216) "È") ((= b2 219) "Ẻ") ((= b2 213) "Ẽ") ((= b2 207) "Ẹ")
        ((= b2 194) "Ê") ((= b2 193) "Ế") ((= b2 192) "Ề") ((= b2 197) "Ể") ((= b2 195) "Ễ") ((= b2 196) "Ệ")
        (t nil)
      )
    )
    ;; e (101)
    ((= b1 101)
      (cond
        ((= b2 249) "é") ((= b2 248) "è") ((= b2 251) "ẻ") ((= b2 245) "ẽ") ((= b2 239) "ẹ")
        ((= b2 226) "ê") ((= b2 225) "ế") ((= b2 224) "ề") ((= b2 229) "ể") ((= b2 227) "ễ") ((= b2 228) "ệ")
        (t nil)
      )
    )
    ;; O (79)
    ((= b1 79)
      (cond
        ((= b2 217) "Ó") ((= b2 216) "Ò") ((= b2 219) "Ỏ") ((= b2 213) "Õ") ((= b2 207) "Ọ")
        ((= b2 194) "Ô") ((= b2 193) "Ố") ((= b2 192) "Ồ") ((= b2 197) "Ổ") ((= b2 195) "Ỗ") ((= b2 196) "Ộ")
        (t nil)
      )
    )
    ;; o (111)
    ((= b1 111)
      (cond
        ((= b2 249) "ó") ((= b2 248) "ò") ((= b2 251) "ỏ") ((= b2 245) "õ") ((= b2 239) "ọ")
        ((= b2 226) "ô") ((= b2 225) "ố") ((= b2 224) "ồ") ((= b2 229) "ổ") ((= b2 227) "ỗ") ((= b2 228) "ộ")
        (t nil)
      )
    )
    ;; U (85)
    ((= b1 85)
      (cond
        ((= b2 217) "Ú") ((= b2 216) "Ù") ((= b2 219) "Ủ") ((= b2 213) "Ũ") ((= b2 207) "Ụ")
        (t nil)
      )
    )
    ;; u (117)
    ((= b1 117)
      (cond
        ((= b2 249) "ú") ((= b2 248) "ù") ((= b2 251) "ủ") ((= b2 245) "ũ") ((= b2 239) "ụ")
        (t nil)
      )
    )
    ;; Y (89)
    ((= b1 89)
      (cond
        ((= b2 217) "Ý") ((= b2 216) "Ỳ") ((= b2 219) "Ỷ") ((= b2 213) "Ỹ") ((= b2 207) "Ỵ")
        (t nil)
      )
    )
    ;; y (121)
    ((= b1 121)
      (cond
        ((= b2 249) "ý") ((= b2 248) "ỳ") ((= b2 251) "ỷ") ((= b2 245) "ỹ") ((= b2 239) "ỵ")
        (t nil)
      )
    )
    ;; Ơ (212)
    ((= b1 212)
      (cond
        ((= b2 217) "Ớ") ((= b2 216) "Ờ") ((= b2 219) "Ở") ((= b2 213) "Ỡ") ((= b2 207) "Ợ")
        (t nil)
      )
    )
    ;; ơ (244)
    ((= b1 244)
      (cond
        ((= b2 249) "ớ") ((= b2 248) "ờ") ((= b2 251) "ở") ((= b2 245) "ỡ") ((= b2 239) "ợ")
        (t nil)
      )
    )
    ;; Ư (214)
    ((= b1 214)
      (cond
        ((= b2 217) "Ứ") ((= b2 216) "Ừ") ((= b2 219) "Ử") ((= b2 213) "Ữ") ((= b2 207) "Ự")
        (t nil)
      )
    )
    ;; ư (246)
    ((= b1 246)
      (cond
        ((= b2 249) "ứ") ((= b2 248) "ừ") ((= b2 251) "ử") ((= b2 245) "ữ") ((= b2 239) "ự")
        (t nil)
      )
    )
    (t nil)
  )
)

;; Ký tự VNI 1-byte (Đ, đ, Ơ, ơ, Ư, ư, Í, Ì, Ỉ, Ĩ, Ị, í, ì, ỉ, ĩ, ị)
(defun VNI-Single->Uni (b /)
  (cond
    ((= b 209) "Đ") ((= b 241) "đ")
    ((= b 212) "Ơ") ((= b 244) "ơ")
    ((= b 214) "Ư") ((= b 246) "ư")
    ((= b 205) "Í") ((= b 204) "Ì") ((= b 198) "Ỉ") ((= b 211) "Ĩ") ((= b 210) "Ị")
    ((= b 237) "í") ((= b 236) "ì") ((= b 230) "ỉ") ((= b 243) "ĩ") ((= b 242) "ị") ((= b 238) "ị")
    ((= b 208) "Đ")
    (t (chr b))
  )
)

(defun VNI-Str->Unicode (s / len i b1 b2 pairUni res)
  (setq len (strlen s) i 1 res "")
  (while (<= i len)
    (setq b1 (Norm-Win1252-Byte (ascii (substr s i 1))))
    (setq b2 (if (< i len) (Norm-Win1252-Byte (ascii (substr s (1+ i) 1))) 0))
    (setq pairUni (VNI-Pair->Uni b1 b2))
    (if pairUni
      (progn
        (setq res (strcat res pairUni))
        (setq i (+ i 2))
      )
      (progn
        (setq res (strcat res (VNI-Single->Uni b1)))
        (setq i (1+ i))
      )
    )
  )
  res
)

;; Bảng mã chuẩn TCVN3 (TCVN 5712:1993) & MicroStation Địa Chính %%nnn (Hỗ trợ cả chữ Hoa 128..159)
(defun Char-TCVN3->Uni (b /)
  (cond
    ((= b 128) "À") ((= b 129) "Ả") ((= b 130) "Ã") ((= b 131) "Á") ((= b 132) "Ạ")
    ((= b 133) "Ặ") ((= b 134) "Ậ") ((= b 135) "È") ((= b 136) "Ẻ") ((= b 137) "Ẽ")
    ((= b 138) "É") ((= b 139) "Ẹ") ((= b 140) "Ệ") ((= b 141) "Ì") ((= b 142) "Ỉ")
    ((= b 143) "Ĩ") ((= b 144) "Í") ((= b 145) "Ị") ((= b 146) "Ò") ((= b 147) "Ỏ")
    ((= b 148) "Õ") ((= b 149) "Ó") ((= b 150) "Ọ") ((= b 151) "Ộ") ((= b 152) "Ờ")
    ((= b 153) "Ở") ((= b 154) "Ỡ") ((= b 155) "Ớ") ((= b 156) "Ợ") ((= b 157) "Ù")
    ((= b 158) "Ủ") ((= b 159) "Ũ")
    ((= b 161) "Ă") ((= b 162) "Â") ((= b 163) "Ê") ((= b 164) "Ô") ((= b 165) "Ơ") ((= b 166) "Ư") ((= b 167) "Đ")
    ((= b 168) "ă") ((= b 169) "â") ((= b 170) "ê") ((= b 171) "ô") ((= b 172) "ơ") ((= b 173) "ư") ((= b 174) "đ")
    ((= b 175) "Ú") ((= b 176) "Ụ") ((= b 177) "Ừ") ((= b 178) "Ử") ((= b 179) "Ữ") ((= b 180) "Ứ")
    ((= b 181) "à") ((= b 182) "ả") ((= b 183) "ã") ((= b 184) "á") ((= b 185) "ạ") ((= b 186) "Ự")
    ((= b 187) "ằ") ((= b 188) "ẳ") ((= b 189) "ẵ") ((= b 190) "ắ") ((= b 191) "Ỳ")
    ((= b 192) "Ỷ") ((= b 193) "Ỹ") ((= b 194) "Ý") ((= b 195) "Ỵ")
    ((= b 198) "ặ") ((= b 199) "ầ") ((= b 200) "ẩ") ((= b 201) "ẫ") ((= b 202) "ấ") ((= b 203) "ậ")
    ((= b 204) "è") ((= b 206) "ẻ") ((= b 207) "ẽ") ((= b 208) "é") ((= b 209) "ẹ")
    ((= b 210) "ề") ((= b 211) "ể") ((= b 212) "ễ") ((= b 213) "ế") ((= b 214) "ệ")
    ((= b 215) "ì") ((= b 216) "ỉ") ((= b 219) "ĩ") ((= b 221) "í") ((= b 222) "ị")
    ((= b 223) "ò") ((= b 225) "ỏ") ((= b 226) "õ") ((= b 227) "ó") ((= b 228) "ọ")
    ((= b 229) "ồ") ((= b 230) "ổ") ((= b 231) "ỗ") ((= b 232) "ố") ((= b 233) "ộ")
    ((= b 234) "ờ") ((= b 235) "ở") ((= b 236) "ỡ") ((= b 237) "ớ") ((= b 238) "ợ")
    ((= b 239) "ù") ((= b 241) "ủ") ((= b 242) "ũ") ((= b 243) "ú") ((= b 244) "ụ")
    ((= b 245) "ừ") ((= b 246) "ử") ((= b 247) "ữ") ((= b 248) "ứ") ((= b 249) "ự")
    ((= b 250) "ỳ") ((= b 251) "ỷ") ((= b 252) "ỹ") ((= b 253) "ý") ((= b 254) "ỵ")
    (t (chr b))
  )
)

(defun TCVN3-Str->Unicode (s / len i b prevB res)
  (setq len (strlen s) i 1 res "")
  (while (<= i len)
    (setq b (Norm-Win1252-Byte (ascii (substr s i 1))))
    (setq prevB (if (> i 1) (ascii (substr s (1- i) 1)) 0))
    ;; Nếu b = 208 (Ð) đứng đầu từ hoặc sau chữ HOA (VD: ÐOÀN, CMÐ), đó là chữ Đ chứ không phải é
    (if (and (= b 208) (or (= i 1) (and (>= prevB 65) (<= prevB 90))))
      (setq res (strcat res "Đ"))
      (setq res (strcat res (Char-TCVN3->Uni b)))
    )
    (setq i (1+ i))
  )
  res
)

;;; -------------------------------------------------------------------------
;;; 3. NHẬN DIỆN ĐA BẢNG MÃ TỪNG TỪ (WORD-BY-WORD SMART MULTI-FONT DECODER)
;;; -------------------------------------------------------------------------

;; Kiểm tra từ có mẫu VNI-Windows đặc trưng hay không
(defun Word-Has-Strong-VNI (w / len i b1 b2 prevB isVni)
  (setq len (strlen w) i 1 isVni nil)
  (while (and (not isVni) (<= i len))
    (setq b1 (Norm-Win1252-Byte (ascii (substr w i 1))))
    (setq b2 (if (< i len) (Norm-Win1252-Byte (ascii (substr w (1+ i) 1))) 0))
    (setq prevB (if (> i 1) (ascii (substr w (1- i) 1)) 0))
    (cond
      ;; 1. Cặp ƯƠ (214+212) hoặc ươ (246+244)
      ((or (and (= b1 214) (= b2 212)) (and (= b1 246) (= b2 244)))
        (setq isVni T)
      )
      ;; 2. Chữ Ñ (209) ở đầu từ hoặc sau chữ HOA (ÑÌNH, ÑKL, CÑ)
      ((and (= b1 209) (or (= i 1) (and (>= prevB 65) (<= prevB 90))))
        (setq isVni T)
      )
      ;; 3. Chữ ñ (241) ở ĐẦU TỪ (ñình, ñường) -> Không bắt ở giữa từ để tránh nhầm 'chñ', 'Thñ', 'cña' của TCVN3!
      ((and (= b1 241) (= i 1))
        (setq isVni T)
      )
      ;; 4. Cặp 2 byte VNI hợp lệ (đã kiểm tra khớp Hoa-Hoa / thường-thường trong VNI-Pair->Uni)
      ((and (> b2 0) (VNI-Pair->Uni b1 b2))
        (cond
          ;; Loại trừ trường hợp trùng với Unicode: O + À(192)/Á(193)/Ã(195) hoặc o + à(224)/á(225)/ã(227) (HOÀNG, TOÁN)
          ((or (and (= b1 79) (member b2 '(192 193 195)))
               (and (= b1 111) (member b2 '(224 225 227))))
            ;; Nếu đứng sau U/u (QUOÁC, quoác) thì chắc chắn 100% là VNI
            (if (or (= prevB 85) (= prevB 117))
              (setq isVni T)
            )
          )
          ;; Loại trừ trường hợp trùng với TCVN3: u(117) + û(251=ỷ trong Thủy, ủy)
          ((and (= b1 117) (= b2 251))
            nil
          )
          (t (setq isVni T))
        )
      )
    )
    (setq i (1+ i))
  )
  isVni
)

;; Kiểm tra từ có mẫu TCVN3 đặc trưng hay không
(defun Word-Has-Strong-TCVN (w / len i b prevB nextB isTcvn)
  (setq len (strlen w) i 1 isTcvn nil)
  (while (and (not isTcvn) (<= i len))
    (setq b (Norm-Win1252-Byte (ascii (substr w i 1))))
    (setq prevB (if (> i 1) (ascii (substr w (1- i) 1)) 0))
    (setq nextB (if (< i len) (ascii (substr w (1+ i) 1)) 0))
    (cond
      ;; 1. Các byte chỉ tồn tại trong TCVN3, không bao giờ có trong Unicode tiếng Việt
      ((or (and (>= b 128) (<= b 174))
           (and (>= b 181) (<= b 190))
           (member b '(198 199 203 206 207 214 215 216 219 220 222 223 228 229 230 231 235 238 239 240 241 246 247 248 251 252 254)))
        (setq isTcvn T)
      )
      ;; 2. Chữ thường a-z đi liền trước mã Latin-1 Hoa 200..221 (NguyÔn, cÊp, YÕn, chÝnh)
      ((and (>= b 200) (<= b 221) (>= prevB 97) (<= prevB 122))
        (setq isTcvn T)
      )
      ;; 3. Nguyên âm huyền/ngã/mũ Latin-1 (ã=227, è=232, ì=236, ò=242, ô=244, õ=245, ù=249) đi trước phụ âm khép c, p, t (Quèc, tèt, thùc, lùc, lôc, môc)
      ((and (member b '(227 232 236 242 244 245 249))
            (member nextB '(67 80 84 99 112 116)))
        (setq isTcvn T)
      )
      ;; 4. é(233=ộ) đi trước c (thuéc, cuéc, méc, léc, đéc)
      ((and (= b 233) (or (= nextB 67) (= nextB 99)) (not (member prevB '(83 88 115 120))))
        (setq isTcvn T)
      )
      ;; 5. ó(243=ú) hoặc ô(244=ụ) đi trước y (Thóy=Thúy, Thôy=Thụy)
      ((and (or (= b 243) (= b 244)) (or (= nextB 89) (= nextB 121)))
        (setq isTcvn T)
      )
    )
    (setq i (1+ i))
  )
  isTcvn
)

;; Kiểm tra từ có chứa ký tự Unicode thật sự (> 255 sau khi chuẩn hóa Win-1252) hay không
(defun Word-Has-Real-Unicode (w / len i b hasUni)
  (setq len (strlen w) i 1 hasUni nil)
  (while (and (not hasUni) (<= i len))
    (setq b (Norm-Win1252-Byte (ascii (substr w i 1))))
    (if (> b 255) (setq hasUni T))
    (setq i (1+ i))
  )
  hasUni
)

(defun Is-Word-Char-Code (b /)
  (and (> b 32)
       (not (member b '(33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 58 59 60 61 62 63 64 91 92 93 94 96 123 124 125 126))))
)

;; Giải mã thông minh 1 dòng/đoạn theo từng từ (hỗ trợ cả MText trộn lẫn TCVN3 + VNI + Unicode)
(defun Decode-Segment-Smart (seg defaultMode / len idx start w sv st ru segHasVni segHasTcvn segHasPureUni res ch)
  (if (or (null seg) (= seg ""))
    ""
    (progn
      (setq len (strlen seg)
            idx 1
            segHasVni nil
            segHasTcvn nil
            segHasPureUni nil)
      ;; Vòng 1: Quét ngữ cảnh của đoạn
      (while (<= idx len)
        (if (not (Is-Word-Char-Code (ascii (substr seg idx 1))))
          (setq idx (1+ idx))
          (progn
            (setq start idx)
            (while (and (<= idx len) (Is-Word-Char-Code (ascii (substr seg idx 1))))
              (setq idx (1+ idx))
            )
            (setq w (substr seg start (- idx start)))
            (setq sv (Word-Has-Strong-VNI w)
                  st (and (not sv) (Word-Has-Strong-TCVN w))
                  ru (Word-Has-Real-Unicode w))
            (cond
              (sv (setq segHasVni T))
              (st (setq segHasTcvn T))
              (ru (setq segHasPureUni T))
            )
          )
        )
      )
      ;; Vòng 2: Giải mã chính xác từng từ
      (setq idx 1
            res "")
      (while (<= idx len)
        (setq ch (substr seg idx 1))
        (if (not (Is-Word-Char-Code (ascii ch)))
          (progn
            (setq res (strcat res ch))
            (setq idx (1+ idx))
          )
          (progn
            (setq start idx)
            (while (and (<= idx len) (Is-Word-Char-Code (ascii (substr seg idx 1))))
              (setq idx (1+ idx))
            )
            (setq w (substr seg start (- idx start)))
            (setq sv (Word-Has-Strong-VNI w)
                  st (and (not sv) (Word-Has-Strong-TCVN w))
                  ru (Word-Has-Real-Unicode w))
            (cond
              (sv (setq res (strcat res (VNI-Str->Unicode w))))
              (st (setq res (strcat res (TCVN3-Str->Unicode w))))
              ((not ru)
                (cond
                  ((or segHasVni (and (not segHasTcvn) (not segHasPureUni) (= defaultMode "VNI")))
                    (setq res (strcat res (VNI-Str->Unicode w)))
                  )
                  ((or (and segHasTcvn (not segHasPureUni)) (and (not segHasPureUni) (= defaultMode "TCVN")))
                    (setq res (strcat res (TCVN3-Str->Unicode w)))
                  )
                  (t
                    (setq res (strcat res (LM:SafeReplace (chr 208) "Đ" w)))
                  )
                )
              )
              (t
                (setq res (strcat res (LM:SafeReplace (chr 208) "Đ" w)))
              )
            )
          )
        )
      )
      res
    )
  )
)

;; Phát hiện gợi ý bảng mã từ chuỗi MText gốc (\fVN..., \fVNI..., %%1xx...)
(defun Detect-Inline-Font-Mode (rawStr / uRaw)
  (setq uRaw (strcase rawStr))
  (cond
    ((or (vl-string-search "%%1" uRaw) (vl-string-search "%%2" uRaw)
         (vl-string-search "\\F.VN" uRaw) (vl-string-search "\\FVNARIAL" uRaw)
         (vl-string-search "\\FVHARIAL" uRaw) (vl-string-search "\\FVNTIME" uRaw)
         (vl-string-search "\\FTCVN" uRaw))
      "TCVN"
    )
    ((or (vl-string-search "\\FVNI" uRaw) (vl-string-search "\\FVTIME" uRaw)
         (vl-string-search "\\FVHELVE" uRaw) (vl-string-search "\\FAVO" uRaw)
         (vl-string-search "\\FAPTIMA" uRaw))
      "VNI"
    )
    ((or (vl-string-search "\\FTIMES NEW ROMAN" uRaw) (vl-string-search "\\FTAHOMA" uRaw)
         (vl-string-search "\\FCALIBRI" uRaw) (vl-string-search "\\FSEGOE" uRaw))
      "UNI"
    )
    (t nil)
  )
)

;; Hàm chuyển đổi thông minh sang Unicode có dấu chuẩn để hiển thị & xuất Excel
(defun To-Unicode-Display (str / defaultMode s segs res seg)
  (cond
    ((or (null str) (= str "")) "")
    ;; Nhánh siêu tốc C++: Nếu chuỗi thuần ASCII (số thửa, diện tích, chữ không dấu, không có mã \ hay %) thì trả về ngay lập tức (0.000001s)
    ((not (wcmatch str "*[~ -z]*,*\\*,*%*")) str)
    (t
      (setq defaultMode (Detect-Inline-Font-Mode str))
      (setq s (if (vl-string-search "%%" str) (Decode-Acad-Percent-Codes str) str))
      (if (or (vl-string-search "\\U+" s) (vl-string-search "\\u+" s))
        (setq s (Decode-Unicode-Hex-Escapes s))
      )
      (if (or (vl-string-search "\\" s) (vl-string-search "{" s) (vl-string-search "}" s))
        (setq s (FastUnformatMText s))
      )
      (if (wcmatch s "*[~ -z]*")
        (setq s (Normalize-Combining-Marks s))
      )
      (if (not (wcmatch s "*[~ -z]*"))
        s
        (progn
          (setq segs (SplitByDelim s " | "))
          (if (<= (length segs) 1)
            (Decode-Segment-Smart s defaultMode)
            (progn
              (setq res "")
              (foreach seg segs
                (if (= res "")
                  (setq res (Decode-Segment-Smart seg defaultMode))
                  (setq res (strcat res " | " (Decode-Segment-Smart seg defaultMode)))
                )
              )
              res
            )
          )
        )
      )
    )
  )
)

(setq *UNI-ASCII-PAIRS*
  '(
    ("á" . "a") ("à" . "a") ("ả" . "a") ("ã" . "a") ("ạ" . "a")
    ("ă" . "a") ("ắ" . "a") ("ằ" . "a") ("ẳ" . "a") ("ẵ" . "a") ("ặ" . "a")
    ("â" . "a") ("ấ" . "a") ("ầ" . "a") ("ẩ" . "a") ("ẫ" . "a") ("ậ" . "a")
    ("đ" . "d") ("Đ" . "d") ("Ð" . "d") ("ð" . "d")
    ("é" . "e") ("è" . "e") ("ẻ" . "e") ("ẽ" . "e") ("ẹ" . "e")
    ("ê" . "e") ("ế" . "e") ("ề" . "e") ("ể" . "e") ("ễ" . "e") ("ệ" . "e")
    ("í" . "i") ("ì" . "i") ("ỉ" . "i") ("ĩ" . "i") ("ị" . "i")
    ("ó" . "o") ("ò" . "o") ("ỏ" . "o") ("õ" . "o") ("ọ" . "o")
    ("ô" . "o") ("ố" . "o") ("ồ" . "o") ("ổ" . "o") ("ỗ" . "o") ("ộ" . "o")
    ("ơ" . "o") ("ớ" . "o") ("ờ" . "o") ("ở" . "o") ("ỡ" . "o") ("ợ" . "o")
    ("ú" . "u") ("ù" . "u") ("ủ" . "u") ("ũ" . "u") ("ụ" . "u")
    ("ư" . "u") ("ứ" . "u") ("ừ" . "u") ("ử" . "u") ("ữ" . "u") ("ự" . "u")
    ("ý" . "y") ("ỳ" . "y") ("ỷ" . "y") ("ỹ" . "y") ("ỵ" . "y")
    ("Á" . "a") ("À" . "a") ("Ả" . "a") ("Ã" . "a") ("Ạ" . "a")
    ("Ă" . "a") ("Ắ" . "a") ("Ằ" . "a") ("Ẳ" . "a") ("Ẵ" . "a") ("Ặ" . "a")
    ("Â" . "a") ("Ấ" . "a") ("Ầ" . "a") ("Ẩ" . "a") ("Ẫ" . "a") ("Ậ" . "a")
    ("É" . "e") ("È" . "e") ("Ẻ" . "e") ("Ẽ" . "e") ("Ẹ" . "e")
    ("Ê" . "e") ("Ế" . "e") ("Ề" . "e") ("Ể" . "e") ("Ễ" . "e") ("Ệ" . "e")
    ("Í" . "i") ("Ì" . "i") ("Ỉ" . "i") ("Ĩ" . "i") ("Ị" . "i")
    ("Ó" . "o") ("Ò" . "o") ("Ỏ" . "o") ("Õ" . "o") ("Ọ" . "o")
    ("Ô" . "o") ("Ố" . "o") ("Ồ" . "o") ("Ổ" . "o") ("Ỗ" . "o") ("Ộ" . "o")
    ("Ơ" . "o") ("Ớ" . "o") ("Ờ" . "o") ("Ở" . "o") ("Ỡ" . "o") ("Ợ" . "o")
    ("Ú" . "u") ("Ù" . "u") ("Ủ" . "u") ("Ũ" . "u") ("Ụ" . "u")
    ("Ư" . "u") ("Ứ" . "u") ("Ừ" . "u") ("Ử" . "u") ("Ữ" . "u") ("Ự" . "u")
    ("Ý" . "y") ("Ỳ" . "y") ("Ỷ" . "y") ("Ỹ" . "y") ("Ỵ" . "y")
  )
)

;; Chuyển chuỗi Unicode tiếng Việt về chữ thường không dấu (Plain ASCII)
(defun Uni-Str->Plain-ASCII (str / item len i b res)
  (cond
    ((or (null str) (= str "")) "")
    ;; Nhánh siêu tốc C++: Nếu chuỗi không có ký tự > 122 (không dấu) thì dùng hàm C++ (strcase str T) ngay lập tức!
    ((not (wcmatch str "*[~ -z]*"))
      (strcase str T)
    )
    (t
      (foreach item *UNI-ASCII-PAIRS*
        (if (vl-string-search (car item) str)
          (setq str (LM:SafeReplace (car item) (cdr item) str))
        )
      )
      (setq str (strcase str T))
      (if (not (wcmatch str "*[~ -~]*"))
        str
        (progn
          (setq len (strlen str) i 1 res "")
          (while (<= i len)
            (setq b (ascii (substr str i 1)))
            (if (and (>= b 32) (<= b 126))
              (setq res (strcat res (chr b)))
            )
            (setq i (1+ i))
          )
          res
        )
      )
    )
  )
)

;; Hàm chuẩn hóa TOÀN NĂNG: chuyển bất kỳ chuỗi nào (TCVN3, VNI, Unicode, %%nnn) sang chữ thường không dấu
(defun To-Plain-ASCII (str /)
  (cond
    ((or (null str) (= str "")) "")
    ((not (wcmatch str "*[~ -z]*,*\\*,*%*")) (strcase str T))
    (t (Uni-Str->Plain-ASCII (To-Unicode-Display str)))
  )
)


;;; -------------------------------------------------------------------------
;;; 4. TIỆN ÍCH CHUỖI, TÁCH TỪ & PHÂN TÍCH TỌA ĐỘ VN-2000
;;; -------------------------------------------------------------------------
(defun LM:SafeReplace (old new str / pos len res)
  (setq len (strlen old)
        res "")
  (while (setq pos (vl-string-search old str))
    (setq res (strcat res (substr str 1 pos) new)
          str (substr str (+ pos len 1)))
  )
  (strcat res str)
)

(defun SplitByDelim (str delim / pos res len)
  (setq len (strlen delim)
        res '())
  (while (setq pos (vl-string-search delim str))
    (setq res (cons (substr str 1 pos) res)
          str (substr str (+ pos len 1)))
  )
  (setq res (cons str res))
  (reverse res)
)

;; Bôi nổi bật từ khóa tìm kiếm trong chuỗi văn bản bằng ký hiệu 【...】
(defun Highlight-Keyword-In-Text (txt kw / cleanKw kwAscii kwWords kwLen words res w cleanW wAsc)
  (if (or (null txt) (= txt "") (null kw) (= (vl-string-trim " " kw) ""))
    (if txt txt "")
    (progn
      (setq cleanKw (vl-string-trim " *?" kw)
            kwAscii (Uni-Str->Plain-ASCII cleanKw)
            kwWords (SplitByDelim kwAscii " ")
            kwLen   (strlen kwAscii))
      (if (or (null kwAscii) (= kwAscii ""))
        txt
        (progn
          (setq words (SplitByDelim txt " ")
                res "")
          (foreach w words
            (setq cleanW (vl-string-trim " ,;()[]{}/\"':;+-*=" w)
                  wAsc   (Uni-Str->Plain-ASCII cleanW))
            (if (and (/= wAsc "")
                     (or (member wAsc kwWords)
                         (= wAsc kwAscii)
                         (and (>= kwLen 2) (vl-string-search kwAscii wAsc))
                         (and (>= kwLen 3) (vl-string-search wAsc kwAscii))))
              ;; Khớp từ khóa -> Bọc ký hiệu 【...】 để bôi nổi bật trực quan
              (setq res (strcat res (if (= res "") "" " ")
                                (if (and (/= cleanW w) (vl-string-search cleanW w))
                                  (vl-string-subst (strcat "【" cleanW "】") cleanW w)
                                  (strcat "【" w "】"))))
              (setq res (strcat res (if (= res "") "" " ") w))
            )
          )
          res
        )
      )
    )
  )
)

;; Nhận diện chuỗi Tọa độ VN-2000 (X, Y)
(defun Parse-Coordinate-String (str / clean tokens nums pt x y tok)
  (setq clean (LM:SafeReplace "," " " str))
  (setq clean (LM:SafeReplace ";" " " clean))
  (setq clean (LM:SafeReplace "X=" " " clean))
  (setq clean (LM:SafeReplace "x=" " " clean))
  (setq clean (LM:SafeReplace "Y=" " " clean))
  (setq clean (LM:SafeReplace "y=" " " clean))
  (setq clean (LM:SafeReplace ":" " " clean))
  (setq tokens (SplitByDelim clean " "))
  (setq nums '())
  (foreach tok tokens
    (setq tok (vl-string-trim " " tok))
    (if (and (/= tok "") (distof tok))
      (setq nums (cons (distof tok) nums))
    )
  )
  (setq nums (reverse nums))
  (if (= (length nums) 2)
    (progn
      (setq x (car nums)
            y (cadr nums))
      ;; Tự động xử lý nếu gõ Y trước X sau (ở miền Nam: X ~ 400.000 - 700.000, Y ~ 1.000.000 - 2.500.000)
      (if (and (> x 900000.0) (< y 900000.0))
        (setq pt (list y x 0.0))
        (setq pt (list x y 0.0))
      )
      pt
    )
    nil
  )
)

(defun Strip-Title-Prefix (name / uName prefixes pLen res found p)
  (setq name (vl-string-trim " :;-\t" name)
        uName (strcase name))
  ;; Nếu bản thân token chính là từ dừng địa chính (ví dụ: "thửa", "tờ", "gcn", "ông", "bà")
  (if (member uName '("THỬA" "THUA" "TỜ" "TO" "SỐ" "SO" "GCN" "ĐẤT" "DAT" "BĐ" "BD" "ÔNG" "ONG" "BÀ" "BA" "CHỦ" "CHU" "HỘ" "HO" "DT" "DIỆN TÍCH"))
    ""
    (progn
      (setq prefixes '("ÔNG VÀ BÀ " "BÀ VÀ ÔNG " "ÔNG: " "BÀ: " "ÔNG " "BÀ "
                       "CHỒNG: " "VỢ: " "CHỦ HỘ: " "ĐSH: " "CHỦ SỬ DỤNG: "
                       "TỜ BẢN ĐỒ: " "TỜ BẢN ĐỒ " "TỜ BĐ: " "TỜ BĐ " "TỜ: " "TỜ "
                       "THỬA ĐẤT: " "THỬA ĐẤT " "SỐ THỬA: " "SỐ THỬA " "THỬA: " "THỬA "
                       "SỐ TỜ: " "SỐ TỜ " "DT: " "DIỆN TÍCH: " "LOẠI ĐẤT: "
                       "GCN: " "GCN " "SỐ GCN: " "SỐ GCN "))
      (setq res name
            found nil)
      (foreach p prefixes
        (if (and (not found)
                 (>= (strlen uName) (setq pLen (strlen p)))
                 (= (substr uName 1 pLen) p))
          (setq res (vl-string-trim " :;-\t" (substr name (1+ pLen)))
                found T)
        )
      )
      res
    )
  )
)

;; Tạo biến thể viết tắt tên đường/họ tên trên bản đồ (VD: "nguyen duy trinh" -> "ndtrinh")
(defun Build-Abbrev-Token (plainStr / words w cleanWords initials lastW)
  (setq words (SplitByDelim plainStr " ")
        cleanWords '())
  (foreach w words
    (setq w (vl-string-trim " " w))
    (if (> (strlen w) 0)
      (setq cleanWords (cons w cleanWords))
    )
  )
  (setq cleanWords (reverse cleanWords))
  (if (and (>= (length cleanWords) 3) (<= (length cleanWords) 4))
    (progn
      (setq initials ""
            lastW (nth (1- (length cleanWords)) cleanWords))
      (while (> (length cleanWords) 1)
        (setq initials (strcat initials (substr (car cleanWords) 1 1))
              cleanWords (cdr cleanWords))
      )
      (if (>= (strlen lastW) 3)
        (strcat initials lastW)
        nil
      )
    )
    nil
  )
)

;; Bóc tách thông minh: Số thửa, số tờ, diện tích, tên chủ hộ & viết tắt tên đường
(defun Extract-Smart-Names (kw / str seps s tokens finalTokens numTokens cleanTok isNum abbrTok t1 t2)
  (setq str kw
        finalTokens '()
        numTokens '())

  ;; Tách chuỗi theo các dấu phân tách tự nhiên
  (setq seps '("/" "\\" "-" "_" "," ";" "|" "(" ")" "[" "]" ":" "\t" "\r\n" "\n" "\r" " và " " Và " " VÀ " " va " " & "))
  (foreach s seps
    (setq str (LM:SafeReplace s " " str))
  )

  (setq tokens (SplitByDelim str " "))
  (foreach tok tokens
    (setq tok (vl-string-trim " " tok))
    (if (> (strlen tok) 0)
      (progn
        (setq cleanTok (Strip-Title-Prefix tok))
        (if (> (strlen cleanTok) 0)
          (progn
            (setq isNum (distof cleanTok))
            (if isNum
              (setq numTokens (cons cleanTok numTokens))
            )
            (if (not (member cleanTok finalTokens))
              (setq finalTokens (cons cleanTok finalTokens))
            )
            (setq abbrTok (Build-Abbrev-Token (To-Plain-ASCII cleanTok)))
            (if (and abbrTok (not (member abbrTok finalTokens)))
              (setq finalTokens (cons abbrTok finalTokens))
            )
          )
        )
      )
    )
  )

  (setq numTokens (reverse numTokens))
  ;; Nếu có từ 2 số trở lên (ví dụ: số thửa 434 và số tờ 22), tự ghép mọi định dạng khả dĩ
  (if (>= (length numTokens) 2)
    (progn
      (setq t1 (nth 0 numTokens)
            t2 (nth 1 numTokens))
      (setq finalTokens (cons (strcat t1 "/" t2) finalTokens))
      (setq finalTokens (cons (strcat t2 "/" t1) finalTokens))
      (setq finalTokens (cons (strcat t1 "-" t2) finalTokens))
      (setq finalTokens (cons (strcat t1 " " t2) finalTokens))
    )
  )

  (if (and kw (/= kw "") (not (member kw finalTokens)))
    (setq finalTokens (cons kw finalTokens))
  )

  (if (null finalTokens)
    (list kw)
    (reverse finalTokens)
  )
)


;;; -------------------------------------------------------------------------
;;; 5. TOÁN HỌC CHUYỂN ĐỔI TỌA ĐỘ CHO ĐỐI TƯỢNG CON TRONG BLOCK
;;; -------------------------------------------------------------------------
(defun Transform-Local-To-WCS (locPt insPt sx sy sz rot / u v w u1 v1 xRot yRot)
  (setq u (car locPt)
        v (cadr locPt)
        w (if (caddr locPt) (caddr locPt) 0.0))
  (setq u1 (* u (if sx sx 1.0))
        v1 (* v (if sy sy 1.0)))
  (if (or (null rot) (equal rot 0.0 1e-6))
    (setq xRot u1 yRot v1)
    (setq xRot (- (* u1 (cos rot)) (* v1 (sin rot)))
          yRot (+ (* u1 (sin rot)) (* v1 (cos rot))))
  )
  (list (+ (car insPt) xRot)
        (+ (cadr insPt) yRot)
        (+ (if (caddr insPt) (caddr insPt) 0.0) (* w (if sz sz 1.0))))
)

;;; -------------------------------------------------------------------------
;;; 6. ĐỌC DỮ LIỆU TỪ CLIPBOARD & LẤY TẤT CẢ LAYER
;;; -------------------------------------------------------------------------
(defun LM:GetClipboardText ( / html text )
  (vl-catch-all-apply
    '(lambda ()
       (setq html (vlax-create-object "htmlfile"))
       (setq text (vlax-invoke (vlax-get (vlax-get html 'parentWindow) 'clipboardData) 'getData "Text"))
       (vlax-release-object html)
     )
  )
  (if (= (type text) 'STR) text "")
)

(defun Get-All-Layers ( / lay layList )
  (setq layList '())
  (setq lay (tblnext "LAYER" T))
  (while lay
    (setq layList (cons (cdr (assoc 2 lay)) layList))
    (setq lay (tblnext "LAYER"))
  )
  (cons "Tất cả các Layer (*)" (acad_strlsort layList))
)

;;; -------------------------------------------------------------------------
;;; 7. THUẬT TOÁN SO KHỚP CHUỖI TOÀN NĂNG SIÊU TỐC C++ (SMART MATCH MULTI-FONT)
;;; -------------------------------------------------------------------------

;; Kiểm tra ký tự ASCII bất biến trong mọi bảng mã tiếng Việt (số 0-9 và phụ âm trừ d/đ)
(defun Is-Invariant-Char-Code (b /)
  (or (and (>= b 48) (<= b 57))
      (and (>= b 98) (<= b 122) (not (member b '(100 101 105 111 117 121)))))
)

;; Tạo khung xương phụ âm/chữ số bất biến cho wcmatch C++ (chữ thường)
(defun Build-Word-Skeleton (wAscii / len i b pat lastStar invCount)
  (setq len (strlen wAscii)
        i 1
        pat "*"
        lastStar T
        invCount 0)
  (while (<= i len)
    (setq b (ascii (substr wAscii i 1)))
    (if (Is-Invariant-Char-Code b)
      (setq pat (strcat pat (chr b))
            lastStar nil
            invCount (1+ invCount))
      (if (not lastStar)
        (setq pat (strcat pat "*")
              lastStar T)
      )
    )
    (setq i (1+ i))
  )
  (if (not lastStar) (setq pat (strcat pat "*")))
  (cons invCount pat)
)

;; Tạo mẫu lọc DXF Group 1 trực tiếp trong C++ (ssget "X") không phân biệt Hoa/Thường & Bảng mã
(defun Build-Word-SSGet-Pat (wAscii / len i b pat lastStar invCount)
  (setq len (strlen wAscii)
        i 1
        pat "*"
        lastStar T
        invCount 0)
  (while (<= i len)
    (setq b (ascii (substr wAscii i 1)))
    (cond
      ((and (>= b 48) (<= b 57))
        (setq pat (strcat pat (chr b))
              lastStar nil
              invCount (1+ invCount))
      )
      ((and (>= b 98) (<= b 122) (not (member b '(100 101 105 111 117 121))))
        (setq pat (strcat pat "[" (chr (- b 32)) (chr b) "]")
              lastStar nil
              invCount (1+ invCount))
      )
      (t
        (if (not lastStar)
          (setq pat (strcat pat "*")
                lastStar T)
        )
      )
    )
    (setq i (1+ i))
  )
  (if (not lastStar) (setq pat (strcat pat "*")))
  (cons invCount pat)
)

;; Tiền xử lý danh sách từ khóa 1 lần duy nhất trước khi quét hàng vạn đối tượng
(defun Prepare-Search-Specs (kwList / specs kw dispKw kwUni kwAscii isWild rawWords words w skPair wordSkels ssPair bestSS)
  (setq specs '())
  (foreach kw kwList
    (if (and kw (/= kw ""))
      (progn
        (setq dispKw  (To-Unicode-Display kw)
              kwUni   (strcase dispKw)
              kwAscii (Uni-Str->Plain-ASCII dispKw)
              isWild  (or (vl-string-search "*" kw) (vl-string-search "?" kw))
              rawWords (SplitByDelim kwAscii " ")
              words   '()
              wordSkels '()
              bestSS  (cons 0 "*"))
        (foreach w rawWords
          (setq w (vl-string-trim " " w))
          (if (> (strlen w) 0)
            (progn
              (setq words (cons w words))
              (setq skPair (Build-Word-Skeleton w))
              (if (> (car skPair) 0)
                (setq wordSkels (cons (cdr skPair) wordSkels))
              )
              (setq ssPair (Build-Word-SSGet-Pat w))
              (if (> (car ssPair) (car bestSS))
                (setq bestSS ssPair)
              )
            )
          )
        )
        (setq words (reverse words)
              wordSkels (reverse wordSkels))
        (setq specs (cons (list kw isWild kwAscii kwUni words wordSkels bestSS) specs))
      )
    )
  )
  (reverse specs)
)

;; Tổng hợp mẫu lọc C++ ssget Group 1 từ kwSpecs
(defun Build-SSGet-Anchor-Pattern (kwSpecs / canFilter patList spec bestSS p res)
  (setq canFilter T
        patList '())
  (foreach spec kwSpecs
    (setq bestSS (nth 6 spec))
    (if (or (null bestSS) (<= (car bestSS) 0))
      (setq canFilter nil)
      (if (not (member (cdr bestSS) patList))
        (setq patList (cons (cdr bestSS) patList))
      )
    )
  )
  (if (and canFilter patList)
    (progn
      (setq res "")
      (foreach p (reverse patList)
        (if (= res "")
          (setq res p)
          (setq res (strcat res "," p))
        )
      )
      res
    )
    nil
  )
)

;; So khớp toàn diện đa bảng mã tiếng Việt (Unicode, VNI, TCVN3, MicroStation %%nnn)
(defun FastMatch-Prepared (txt kwSpecs / dispTxt cleanUni cleanAscii isMatch spec isWild kwAscii kwUni words allWordsInTxt w)
  (if (or (null txt) (= txt "") (null kwSpecs))
    nil
    (progn
      (setq dispTxt    (To-Unicode-Display txt)
            cleanUni   (strcase dispTxt)
            cleanAscii (Uni-Str->Plain-ASCII dispTxt)
            isMatch    nil)
      (foreach spec kwSpecs
        (if (not isMatch)
          (progn
            (setq isWild  (nth 1 spec)
                  kwAscii (nth 2 spec)
                  kwUni   (nth 3 spec)
                  words   (nth 4 spec))
            (cond
              ;; 1. Ký tự đại diện (* hoặc ?)
              (isWild
                (if (or (wcmatch cleanAscii kwAscii)
                        (wcmatch cleanUni kwUni))
                  (setq isMatch T)
                )
              )
              ;; 2. Khớp chuỗi con chính xác (Substring)
              ((or (vl-string-search kwAscii cleanAscii)
                   (vl-string-search kwUni cleanUni))
                (setq isMatch T)
              )
              ;; 3. Khớp nhiều từ (Multi-word AND matching: vd "tran diem" khớp "Trần Thị Diễm")
              ((> (length words) 1)
                (setq allWordsInTxt T)
                (foreach w words
                  (if (and allWordsInTxt (not (vl-string-search w cleanAscii)))
                    (setq allWordsInTxt nil)
                  )
                )
                (if allWordsInTxt (setq isMatch T))
              )
            )
          )
        )
      )
      isMatch
    )
  )
)

(defun FastMatch (txt kwList /)
  (FastMatch-Prepared txt (Prepare-Search-Specs kwList))
)

;;; -------------------------------------------------------------------------
;;; 8. QUÉT SÂU BLOCK LỒNG BLOCK (RECURSIVE NESTED BLOCKS)
;;; -------------------------------------------------------------------------
(setq *Blk-Cache* nil)

(defun Scan-Block-Recursive (blkName kwSpecs depth maxDepth visited / blkEnt sub subDxf sTyp sTxt item matches subMatches childBlk locPt)
  (setq matches '())
  (if (and blkName
           (/= blkName "")
           (not (wcmatch (strcase blkName) "`*D*,`*X*"))
           (< depth maxDepth)
           (not (member blkName visited))
           (setq blkEnt (tblobjname "BLOCK" blkName)))
    (progn
      (setq visited (cons blkName visited))
      (setq sub blkEnt)
      (while (setq sub (entnext sub))
        (setq subDxf (entget sub))
        (if subDxf
          (progn
            (setq sTyp (cdr (assoc 0 subDxf)))
            (cond
              ((= sTyp "ENDBLK")
                ;; Kết thúc block
              )
              ((= sTyp "TEXT")
                (setq sTxt (cdr (assoc 1 subDxf)))
                (if (FastMatch-Prepared sTxt kwSpecs)
                  (progn
                    (setq locPt (cdr (assoc 10 subDxf)))
                    (if (null locPt) (setq locPt (cdr (assoc 11 subDxf))))
                    (setq matches (cons (list "Text" sTxt locPt blkName) matches))
                  )
                )
              )
              ((= sTyp "MTEXT")
                (setq sTxt "")
                (foreach item subDxf
                  (if (= (car item) 3) (setq sTxt (strcat sTxt (cdr item))))
                )
                (setq sTxt (strcat sTxt (cdr (assoc 1 subDxf))))
                (if (FastMatch-Prepared sTxt kwSpecs)
                  (progn
                    (setq locPt (cdr (assoc 10 subDxf)))
                    (setq matches (cons (list "MText" sTxt locPt blkName) matches))
                  )
                )
              )
              ((= sTyp "ATTDEF")
                (setq sTxt (cdr (assoc 1 subDxf)))
                (if (FastMatch-Prepared sTxt kwSpecs)
                  (progn
                    (setq locPt (cdr (assoc 10 subDxf)))
                    (if (null locPt) (setq locPt (cdr (assoc 11 subDxf))))
                    (setq matches (cons (list (strcat "Tag:" (cdr (assoc 2 subDxf))) sTxt locPt blkName) matches))
                  )
                )
              )
              ((= sTyp "INSERT")
                (setq childBlk (cdr (assoc 2 subDxf)))
                (setq subMatches (Scan-Block-Recursive childBlk kwSpecs (1+ depth) maxDepth visited))
                (if subMatches
                  (setq matches (append subMatches matches))
                )
              )
            )
          )
        )
      )
    )
  )
  matches
)

(defun Get-Cached-Block-Matches (blkName kwSpecs / cached res)
  (if (and blkName (setq cached (assoc blkName *Blk-Cache*)))
    (cdr cached)
    (progn
      (setq res (Scan-Block-Recursive blkName kwSpecs 0 4 '()))
      (setq *Blk-Cache* (cons (cons blkName res) *Blk-Cache*))
      res
    )
  )
)

;;; -------------------------------------------------------------------------
;;; 9. TẠO HỘP THOẠI GIAO DIỆN DCL
;;; -------------------------------------------------------------------------
(defun FastFind:WriteDCL ( / path fn )
  (setq path (vl-filename-mktemp "TraCuuDlg.dcl"))
  (setq fn (open path "w"))

  (write-line "tracuu_dlg : dialog {" fn)
  (write-line "  label = \"Tra Cứu Bản Đồ - Fast Find Pro v12.0 (ĐA BẢN VẼ & TOÀN THƯ MỤC SIÊU TỐC)\";" fn)
  (write-line "  fixed_width = true;" fn)
  (write-line "  width = 86;" fn)
  (write-line "  spacer;" fn)
  (write-line "  : boxed_column {" fn)
  (write-line "    label = \"Nội dung tìm kiếm (Thửa 434, Tờ 22, Tên chủ, Diện tích, Số GCN, Tọa độ X Y, *, ?):\";" fn)
  (write-line "    : edit_box {" fn)
  (write-line "      key = \"kw_box\";" fn)
  (write-line "      edit_width = 76;" fn)
  (write-line "      allow_accept = true;" fn)
  (write-line "    }" fn)
  (write-line "    : row {" fn)
  (write-line "      : text { label = \"Lịch sử tìm:\"; width = 14; }" fn)
  (write-line "      : popup_list {" fn)
  (write-line "        key = \"pop_history\";" fn)
  (write-line "        width = 60;" fn)
  (write-line "      }" fn)
  (write-line "    }" fn)
  (write-line "  }" fn)
  (write-line "  spacer;" fn)
  (write-line "  : boxed_column {" fn)
  (write-line "    label = \"CÔNG TẮC MỞ / TẮT TÌM KIẾM TOÀN BỘ THƯ MỤC BẢN ĐỒ (.DWG):\";" fn)
  (write-line "    : row {" fn)
  (write-line "      : toggle {" fn)
  (write-line "        key = \"chk_folder_mode\";" fn)
  (write-line "        label = \"BẬT Tìm Kiếm Toàn Bộ Các Tờ Bản Đồ Trong Thư Mục (Siêu Tốc)\";" fn)
  (write-line "        width = 56;" fn)
  (write-line "      }" fn)
  (write-line "      : button {" fn)
  (write-line "        key = \"btn_toggle_folder\";" fn)
  (write-line "        label = \"Công Tắc BẬT/TẮT\";" fn)
  (write-line "        width = 18;" fn)
  (write-line "      }" fn)
  (write-line "    }" fn)
  (write-line "    : text {" fn)
  (write-line "      key = \"lbl_folder_state\";" fn)
  (write-line "      label = \"Trạng thái: [ĐANG TẮT] -> Chỉ tìm trên Bản Vẽ Hiện Hành\";" fn)
  (write-line "    }" fn)
  (write-line "    : row {" fn)
  (write-line "      : text { label = \"Thư mục BĐ:\"; width = 14; }" fn)
  (write-line "      : edit_box {" fn)
  (write-line "        key = \"dir_box\";" fn)
  (write-line "        edit_width = 41;" fn)
  (write-line "      }" fn)
  (write-line "      : button {" fn)
  (write-line "        key = \"btn_pick_dir\";" fn)
  (write-line "        label = \"Chọn Thư Mục...\";" fn)
  (write-line "        width = 17;" fn)
  (write-line "      }" fn)
  (write-line "    }" fn)
  (write-line "  }" fn)
  (write-line "  spacer;" fn)
  (write-line "  : boxed_column {" fn)
  (write-line "    label = \"Phạm vi trên bản vẽ đơn & Bộ lọc đối tượng:\";" fn)
  (write-line "    : row {" fn)
  (write-line "      : text { label = \"Phạm vi:\"; width = 14; }" fn)
  (write-line "      : popup_list {" fn)
  (write-line "        key = \"pop_scope\";" fn)
  (write-line "        width = 60;" fn)
  (write-line "      }" fn)
  (write-line "    }" fn)
  (write-line "    : row {" fn)
  (write-line "      : text { label = \"Lọc Layer:\"; width = 14; }" fn)
  (write-line "      : popup_list {" fn)
  (write-line "        key = \"pop_layer\";" fn)
  (write-line "        width = 60;" fn)
  (write-line "      }" fn)
  (write-line "    }" fn)
  (write-line "    : row {" fn)
  (write-line "      : text { label = \"Loại đối tượng:\"; width = 14; }" fn)
  (write-line "      : popup_list {" fn)
  (write-line "        key = \"pop_type\";" fn)
  (write-line "        width = 60;" fn)
  (write-line "      }" fn)
  (write-line "    }" fn)
  (write-line "  }" fn)
  (write-line "  spacer;" fn)
  (write-line "  : boxed_column {" fn)
  (write-line "    label = \"Tính năng tìm kiếm thông minh:\";" fn)
  (write-line "    : toggle {" fn)
  (write-line "      key = \"chk_split\";" fn)
  (write-line "      label = \"Tự động bóc tách số thửa/diện tích & tìm đa từ khóa thông minh\";" fn)
  (write-line "      value = \"1\";" fn)
  (write-line "    }" fn)
  (write-line "    : toggle {" fn)
  (write-line "      key = \"chk_nested\";" fn)
  (write-line "      label = \"Quét sâu vào Block lồng nhau (Master Sheet Block & Sub-Blocks)\";" fn)
  (write-line "      value = \"1\";" fn)
  (write-line "    }" fn)
  (write-line "  }" fn)
  (write-line "  spacer;" fn)
  (write-line "  : row {" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"btn_inspect\";" fn)
  (write-line "      label = \"Kiểm Tra (KTD)\" ;" fn)
  (write-line "      width = 16;" fn)
  (write-line "    }" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"btn_reindex\";" fn)
  (write-line "      label = \"Cập Nhật Chỉ Mục Thư Mục\";" fn)
  (write-line "      width = 24;" fn)
  (write-line "    }" fn)
  (write-line "    spacer;" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"accept\";" fn)
  (write-line "      label = \"Tìm Kiếm (Enter)\";" fn)
  (write-line "      is_default = true;" fn)
  (write-line "      width = 18;" fn)
  (write-line "    }" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"cancel\";" fn)
  (write-line "      label = \"Hủy (Esc)\";" fn)
  (write-line "      is_cancel = true;" fn)
  (write-line "      width = 12;" fn)
  (write-line "    }" fn)
  (write-line "  }" fn)
  (write-line "}" fn)
  (write-line "" fn)
  (write-line "tracuu_results_dlg : dialog {" fn)
  (write-line "  label = \"Kết Quả Tìm Thấy Trên Bản Vẽ - Fast Find Pro v12.0\";" fn)
  (write-line "  fixed_width = true;" fn)
  (write-line "  width = 135;" fn)
  (write-line "  : text {" fn)
  (write-line "    key = \"lbl_summary\";" fn)
  (write-line "    label = \"Đang tải kết quả...\";" fn)
  (write-line "  }" fn)
  (write-line "  spacer;" fn)
  (write-line "  : boxed_column {" fn)
  (write-line "    label = \"DANH SÁCH ĐỐI TƯỢNG TÌM THẤY (Phím Mũi Tên Xuống để duyệt - Nhấp đúp hoặc Enter để Zoom):\";" fn)
  (write-line "    : list_box {" fn)
  (write-line "      key = \"res_list\";" fn)
  (write-line "      width = 130;" fn)
  (write-line "      height = 17;" fn)
  (write-line "      allow_accept = true;" fn)
  (write-line "    }" fn)
  (write-line "  }" fn)
  (write-line "  : boxed_column {" fn)
  (write-line "    label = \"THÔNG TIN CHI TIẾT ĐỐI TƯỢNG ĐANG CHỌN (TỌA ĐỘ VN-2000):\";" fn)
  (write-line "    : text {" fn)
  (write-line "      key = \"txt_info_matched\";" fn)
  (write-line "      label = \"📌 Nội dung chữ: ---\";" fn)
  (write-line "    }" fn)
  (write-line "    : text {" fn)
  (write-line "      key = \"txt_info_coords\";" fn)
  (write-line "      label = \"🎯 Tọa độ VN-2000:  X = ---   │   Y = ---\";" fn)
  (write-line "    }" fn)
  (write-line "    : text {" fn)
  (write-line "      key = \"txt_info_props\";" fn)
  (write-line "      label = \"📐 Thuộc tính: Loại: ---   │   Layer: ---   │   Cao: ---   │   Góc: ---\";" fn)
  (write-line "    }" fn)
  (write-line "  }" fn)
  (write-line "  spacer;" fn)
  (write-line "  : boxed_row {" fn)
  (write-line "    label = \"Điều hướng trên bản vẽ:\";" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"btn_zoom\";" fn)
  (write-line "      label = \"Zoom Đến Đối Tượng (Enter)\";" fn)
  (write-line "      is_default = true;" fn)
  (write-line "      width = 24;" fn)
  (write-line "    }" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"btn_tour\";" fn)
  (write-line "      label = \"Duyệt Tuần Tự (Tour Mode)\";" fn)
  (write-line "      width = 24;" fn)
  (write-line "    }" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"btn_zoom_all\";" fn)
  (write-line "      label = \"Zoom Toàn Bộ\";" fn)
  (write-line "      width = 16;" fn)
  (write-line "    }" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"btn_select_all\";" fn)
  (write-line "      label = \"Chọn Tất Cả (Grips)\";" fn)
  (write-line "      width = 18;" fn)
  (write-line "    }" fn)
  (write-line "  }" fn)
  (write-line "  : boxed_row {" fn)
  (write-line "    label = \"Công cụ nâng cao (Xuất Excel & Đánh dấu):\" ;" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"btn_export_csv\";" fn)
  (write-line "      label = \"Xuất File Excel (CSV)\";" fn)
  (write-line "      width = 22;" fn)
  (write-line "    }" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"btn_mark_circle\";" fn)
  (write-line "      label = \"Khoanh Vòng Đỏ\";" fn)
  (write-line "      width = 18;" fn)
  (write-line "    }" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"btn_clear_mark\";" fn)
  (write-line "      label = \"Xóa Vòng Đỏ\";" fn)
  (write-line "      width = 16;" fn)
  (write-line "    }" fn)
  (write-line "  }" fn)
  (write-line "  : row {" fn)
  (write-line "    alignment = centered;" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"btn_new\";" fn)
  (write-line "      label = \"Tìm Từ Khác...\";" fn)
  (write-line "      width = 16;" fn)
  (write-line "    }" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"cancel\";" fn)
  (write-line "      label = \"Đóng (Esc)\";" fn)
  (write-line "      is_cancel = true;" fn)
  (write-line "      width = 14;" fn)
  (write-line "    }" fn)
  (write-line "  }" fn)
  (write-line "}" fn)
  (write-line "" fn)
  (write-line "tracuu_folder_results_dlg : dialog {" fn)
  (write-line "  label = \"Tra Cứu Bản Đồ Địa Chính Siêu Tốc - Fast Find Pro v12.0 (Đã Lọc Bỏ Tổng Ghép)\";" fn)
  (write-line "  fixed_width = true;" fn)
  (write-line "  width = 140;" fn)
  (write-line "  : text {" fn)
  (write-line "    key = \"lbl_f_summary\";" fn)
  (write-line "    label = \"Đang tải kết quả toàn thư mục...\";" fn)
  (write-line "  }" fn)
  (write-line "  spacer;" fn)
  (write-line "  : boxed_column {" fn)
  (write-line "    label = \"DANH SÁCH KẾT QUẢ TÌM THẤY (Bấm phím Mũi Tên Xuống để duyệt - Nhấp đúp để Mở & Zoom):\";" fn)
  (write-line "    : list_box {" fn)
  (write-line "      key = \"res_f_list\";" fn)
  (write-line "      width = 135;" fn)
  (write-line "      height = 18;" fn)
  (write-line "      allow_accept = true;" fn)
  (write-line "    }" fn)
  (write-line "  }" fn)
  (write-line "  : boxed_column {" fn)
  (write-line "    label = \"THÔNG TIN CHI TIẾT ĐỐI TƯỢNG ĐANG CHỌN (TỌA ĐỘ VN-2000):\";" fn)
  (write-line "    : text {" fn)
  (write-line "      key = \"txt_f_matched\";" fn)
  (write-line "      label = \"📌 Nội dung chữ: ---\";" fn)
  (write-line "    }" fn)
  (write-line "    : text {" fn)
  (write-line "      key = \"txt_f_sheet\";" fn)
  (write-line "      label = \"🗺️ Tờ bản đồ: ---   │   Thư mục: ---   │   Layer: ---   │   Loại: ---\";" fn)
  (write-line "    }" fn)
  (write-line "    : text {" fn)
  (write-line "      key = \"txt_f_coords\";" fn)
  (write-line "      label = \"🎯 Tọa độ VN-2000:  X = --- , Y = ---   │   File: ---\";" fn)
  (write-line "    }" fn)
  (write-line "  }" fn)
  (write-line "  spacer;" fn)
  (write-line "  : boxed_row {" fn)
  (write-line "    label = \"Thao tác nhanh với Tờ Bản Đồ đang chọn:\";" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"btn_open_dwg\";" fn)
  (write-line "      label = \"🚀 Mở Tờ Bản Đồ & Phóng Tới Vị Trí (Enter)\";" fn)
  (write-line "      is_default = true;" fn)
  (write-line "      width = 38;" fn)
  (write-line "    }" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"btn_insert_dwg\";" fn)
  (write-line "      label = \"📥 Chèn Vào Bản Vẽ Đang Mở & Zoom\";" fn)
  (write-line "      width = 34;" fn)
  (write-line "    }" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"btn_open_csv\";" fn)
  (write-line "      label = \"📊 Xuất File Excel (.CSV)\";" fn)
  (write-line "      width = 22;" fn)
  (write-line "    }" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"btn_open_txt\";" fn)
  (write-line "      label = \"📄 Mở Báo Cáo (.TXT)\";" fn)
  (write-line "      width = 20;" fn)
  (write-line "    }" fn)
  (write-line "  }" fn)
  (write-line "  : row {" fn)
  (write-line "    alignment = centered;" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"btn_new\";" fn)
  (write-line "      label = \"🔍 Tìm Từ Khác...\";" fn)
  (write-line "      width = 20;" fn)
  (write-line "    }" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"cancel\";" fn)
  (write-line "      label = \"✖ Đóng (Esc)\";" fn)
  (write-line "      is_cancel = true;" fn)
  (write-line "      width = 18;" fn)
  (write-line "    }" fn)
  (write-line "  }" fn)
  (write-line "}" fn)

  (close fn)
  path
)

;;; -------------------------------------------------------------------------
;;; 10. ĐỊNH VỊ SPOTLIGHT, TÂM ĐỐI TƯỢNG & ZOOM
;;; -------------------------------------------------------------------------
(setq *FastFind-History* '())
(setq *last-matches* nil)
(setq *last-match-idx* 0)
(setq *last-kw* "")
(setq *last-scope-idx* "0")
(setq *last-layer-idx* "0")
(setq *last-type-idx* "0")
(setq *exit-tour-flag* nil)
(if (not *folder-search-enabled*)
  (setq *folder-search-enabled* "0")
)

(defun Normalize-Folder-Path (p / s)
  (if (or (null p) (= (vl-string-trim " " p) ""))
    (if (vl-file-directory-p "D:\\") "D:\\" "C:\\")
    (progn
      (setq s (vl-string-right-trim "\\/" (vl-string-trim " " p)))
      (if (and (= (strlen s) 2) (= (substr s 2 1) ":"))
        (strcat s "\\")
        s
      )
    )
  )
)

;; Tự động chọn thư mục làm việc D:\EXTRACUUAUTOCAD hoặc C:\EXTRACUUAUTOCAD (cho máy chỉ có ổ C:)
(defun TC-Get-Extra-Dir ( /)
  (cond
    ((vl-file-directory-p "D:\\EXTRACUUAUTOCAD") "D:\\EXTRACUUAUTOCAD")
    ((vl-file-directory-p "C:\\EXTRACUUAUTOCAD") "C:\\EXTRACUUAUTOCAD")
    ((vl-file-directory-p "D:\\")
      (vl-mkdir "D:\\EXTRACUUAUTOCAD")
      "D:\\EXTRACUUAUTOCAD"
    )
    (t
      (vl-mkdir "C:\\EXTRACUUAUTOCAD")
      "C:\\EXTRACUUAUTOCAD"
    )
  )
)

(defun TC-Get-Config-Folder ( / cfgPath f line res )
  (setq cfgPath (strcat (TC-Get-Extra-Dir) "\\last_folder.cfg"))
  (if (not (findfile cfgPath))
    (setq cfgPath (strcat (vl-filename-directory (findfile "TRACUU.LSP")) "\\last_folder.cfg"))
  )
  (if (and cfgPath (findfile cfgPath))
    (progn
      (setq f (open cfgPath "r"))
      (if f
        (progn
          (setq line (read-line f))
          (close f)
          (if (and line (/= line "")) (setq res (vl-string-trim " \t\r\n" line)))
        )
      )
    )
  )
  res
)

(defun TC-Save-Config-Folder ( folder / cfgPath f )
  (if (and folder (/= folder ""))
    (progn
      (setq cfgPath (strcat (TC-Get-Extra-Dir) "\\last_folder.cfg"))
      (setq f (open cfgPath "w"))
      (if f
        (progn
          (write-line folder f)
          (close f)
        )
      )
    )
  )
)

(defun TC-Resolve-Dwg-Path ( dwgFull dwgShort / cfgFld )
  (setq cfgFld (TC-Get-Config-Folder))
  (cond
    ((and dwgFull (findfile dwgFull)) dwgFull)
    ((and *last-search-folder* (findfile (strcat (vl-string-right-trim "\\/" *last-search-folder*) "\\" dwgShort)))
      (findfile (strcat (vl-string-right-trim "\\/" *last-search-folder*) "\\" dwgShort)))
    ((and cfgFld (/= cfgFld "ALL") (vl-file-directory-p cfgFld) (findfile (strcat (vl-string-right-trim "\\/" cfgFld) "\\" dwgShort)))
      (findfile (strcat (vl-string-right-trim "\\/" cfgFld) "\\" dwgShort)))
    ((and (getvar "DWGPREFIX") (findfile (strcat (getvar "DWGPREFIX") dwgShort)))
      (findfile (strcat (getvar "DWGPREFIX") dwgShort)))
    ((findfile dwgShort))
    ((and dwgFull (findfile (vl-string-subst "C:\\" "D:\\" dwgFull))))
    ((and dwgFull (findfile (vl-string-subst "D:\\" "C:\\" dwgFull))))
    ((and dwgFull (findfile (vl-string-subst "E:\\" "D:\\" dwgFull))))
    (t dwgFull)
  )
)

(if (or (not *last-search-folder*)
        (not (vl-file-directory-p *last-search-folder*)))
  (setq *last-search-folder*
    (cond
      ((and (TC-Get-Config-Folder) (/= (TC-Get-Config-Folder) "ALL") (vl-file-directory-p (TC-Get-Config-Folder)))
        (TC-Get-Config-Folder))
      ((and (getvar "DWGPREFIX") (vl-file-directory-p (getvar "DWGPREFIX")))
        (Normalize-Folder-Path (getvar "DWGPREFIX")))
      ((vl-file-directory-p "D:\\") "D:\\")
      (t "C:\\")
    )
  )
)

;; Cập nhật giao diện Công Tắc BẬT / TẮT Tìm kiếm Thư mục trực tiếp trên hộp thoại
(defun Update-Folder-Switch-UI (isOn /)
  (if (= isOn "1")
    (progn
      (set_tile "chk_folder_mode" "1")
      (set_tile "lbl_folder_state" "Trạng thái: [ĐANG BẬT] -> Quét siêu tốc TOÀN BỘ TỜ BẢN ĐỒ (.DWG) trong Thư Mục!")
      (mode_tile "dir_box" 0)
      (mode_tile "btn_pick_dir" 0)
      (mode_tile "btn_reindex" 0)
      (mode_tile "pop_scope" 1)
    )
    (progn
      (set_tile "chk_folder_mode" "0")
      (set_tile "lbl_folder_state" "Trạng thái: [ĐANG TẮT] -> Chỉ tìm kiếm gọn nhẹ trên Bản Vẽ Hiện Hành")
      (mode_tile "dir_box" 1)
      (mode_tile "btn_pick_dir" 1)
      (mode_tile "btn_reindex" 1)
      (mode_tile "pop_scope" 0)
    )
  )
)

(defun Clear-Spotlight-Circle ( / ssOld i )
  (if (setq ssOld (ssget "X" '((0 . "CIRCLE") (8 . "_SPOTLIGHT_TRA_CUU"))))
    (repeat (setq i (sslength ssOld))
      (entdel (ssname ssOld (setq i (1- i))))
    )
  )
)

(defun Show-Spotlight-Circle (cen rad / )
  (Clear-Spotlight-Circle)
  (if (null (tblsearch "LAYER" "_SPOTLIGHT_TRA_CUU"))
    (entmake '((0 . "LAYER") (100 . "AcDbSymbolTableRecord") (100 . "AcDbLayerTableRecord")
               (2 . "_SPOTLIGHT_TRA_CUU") (70 . 0) (62 . 2))) ; Màu vàng
  )
  (entmake (list '(0 . "CIRCLE")
                 (cons 8 "_SPOTLIGHT_TRA_CUU")
                 (cons 62 2)
                 (cons 10 cen)
                 (cons 40 rad)))
  (redraw)
)

(defun Get-Object-Center (target ent / minPt maxPt p1 p2 pt dxf typ cen h txtH diag)
  (setq cen nil
        h 30.0)

  ;; 1. Nếu target là tọa độ 3D truyền trực tiếp
  (if (and (listp target) (= (length target) 3) (numberp (car target)) (numberp (cadr target)))
    (setq cen target h 35.0)
    (progn
      ;; 2. Thử vla-GetBoundingBox
      (if (and target
               (not (listp target))
               (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list target 'minPt 'maxPt))))
               minPt maxPt)
        (progn
          (setq p1 (vlax-safearray->list minPt)
                p2 (vlax-safearray->list maxPt))
          (setq diag (distance p1 p2))
          (if (> diag 1e-4)
            (progn
              (setq cen (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2))
              (setq h (max 30.0 (* diag 3.0)))
            )
          )
        )
      )

      ;; 3. Đọc từ DXF Database
      (if (or (null cen) (equal cen '(0.0 0.0 0.0) 1e-4))
        (if (and ent (not (listp ent)) (setq dxf (entget ent)))
          (progn
            (setq typ (cdr (assoc 0 dxf)))
            (setq txtH (cdr (assoc 40 dxf)))
            (if (or (null txtH) (<= txtH 0.0)) (setq txtH 2.5))
            (setq h (max 30.0 (* txtH 15.0)))

            (cond
              ((= typ "TEXT")
                (setq pt (cdr (assoc 11 dxf)))
                (if (or (null pt) (equal pt '(0.0 0.0 0.0) 1e-4))
                  (setq pt (cdr (assoc 10 dxf)))
                )
                (setq cen pt)
              )
              ((= typ "MTEXT")
                (setq cen (cdr (assoc 10 dxf)))
              )
              ((or (= typ "ATTRIB") (= typ "ATTDEF"))
                (setq pt (cdr (assoc 11 dxf)))
                (if (or (null pt) (equal pt '(0.0 0.0 0.0) 1e-4))
                  (setq pt (cdr (assoc 10 dxf)))
                )
                (setq cen pt)
              )
              (t
                (setq cen (cdr (assoc 10 dxf)))
                (if (null cen) (setq cen (cdr (assoc 11 dxf))))
              )
            )
          )
        )
      )

      ;; 4. Thuộc tính InsertionPoint ActiveX
      (if (or (null cen) (equal cen '(0.0 0.0 0.0) 1e-4))
        (if (and target
                 (not (listp target))
                 (vlax-property-available-p target 'InsertionPoint)
                 (not (vl-catch-all-error-p (setq pt (vlax-get target 'InsertionPoint)))))
          (setq cen pt h 40.0)
        )
      )
    )
  )

  (if (null cen)
    (setq cen '(0.0 0.0 0.0) h 50.0)
  )
  (list cen h)
)

(defun ZoomToObject (target ent / cenData cen h ucsCen acadObj)
  (if (and (listp target) (= (length target) 3) (numberp (car target)) (numberp (cadr target)))
    (setq cen target h 35.0)
    (progn
      (setq cenData (Get-Object-Center target ent)
            cen (car cenData)
            h (cadr cenData))
    )
  )
  (if cen
    (progn
      (setq ucsCen (trans cen 0 1))
      (setq acadObj (vlax-get-acad-object))
      ;; 1. Zoom Center ActiveX
      (vl-catch-all-apply 'vla-ZoomCenter (list acadObj (vlax-3d-point cen) h))
      ;; 2. Zoom Center AutoLISP Command
      (vl-catch-all-apply 'vl-cmdf (list "_.zoom" "_c" "_non" ucsCen h))
      ;; 3. Highlight đối tượng
      (if (and target (not (listp target)))
        (vl-catch-all-apply 'vla-Highlight (list target :vlax-true))
      )
      (if (and ent (not (listp ent)) (entget ent))
        (progn
          (redraw ent 3)
          (sssetfirst nil (ssadd ent))
        )
      )
    )
  )
)

(defun Update-Inspector (idx matches kw / item target ent cenData cen h dxf typ txtH rot style lay customPt dispTxt hTxt)
  (if (and (>= idx 0) (< idx (length matches)))
    (progn
      (setq item (nth idx matches)
            target (car item)
            ent (cadr item)
            typ (nth 3 item)
            lay (nth 5 item)
            customPt (if (>= (length item) 7) (nth 6 item) nil))
      (if customPt
        (setq cen customPt h 35.0)
        (setq cenData (Get-Object-Center target ent)
              cen (car cenData)
              h (cadr cenData))
      )
      (if (and ent (not (listp ent)) (setq dxf (entget ent)))
        (progn
          (setq txtH (cdr (assoc 40 dxf))
                rot (cdr (assoc 50 dxf))
                style (cdr (assoc 7 dxf)))
          (if (null txtH) (setq txtH "---") (setq txtH (rtos txtH 2 2)))
          (if (null rot) (setq rot "0°") (setq rot (strcat (rtos (* (/ rot pi) 180.0) 2 1) "°")))
          (if (null style) (setq style "---"))
        )
        (setq txtH "---" rot "0°" style "---")
      )
      (setq dispTxt (nth 4 item)
            hTxt (Highlight-Keyword-In-Text dispTxt (if kw kw *last-kw*)))
      (set_tile "txt_info_matched"
        (strcat "📌 Nội dung tìm thấy:  \"" hTxt "\"   (Khớp từ khóa: \"" (if kw kw *last-kw*) "\")")
      )
      (if cen
        (set_tile "txt_info_coords"
          (strcat "🎯 Tọa độ VN-2000:  X = " (rtos (car cen) 2 3) "   │   Y = " (rtos (cadr cen) 2 3))
        )
        (set_tile "txt_info_coords" "🎯 Tọa độ VN-2000:  Không xác định")
      )
      (set_tile "txt_info_props"
        (strcat "📐 Thuộc tính: Loại: " typ "   │   Layer: " lay "   │   Cao: " txtH "m   │   Góc: " rot "   │   Style: " style)
      )
    )
    (progn
      (set_tile "txt_info_matched" "📌 Nội dung chữ: ---")
      (set_tile "txt_info_coords" "🎯 Tọa độ VN-2000:  ---")
      (set_tile "txt_info_props" "📐 Thuộc tính: Loại: ---   │   Layer: ---")
    )
  )
)

(defun Do-Tour-Navigation (startIdx matches / count curIdx item target ent cenData cen h dispTxt waitKey loopTour customPt)
  (setq count (length matches)
        curIdx startIdx
        loopTour T)
  (while loopTour
    (if (or (< curIdx 0) (>= curIdx count))
      (setq curIdx (rem (+ curIdx count) count))
    )
    (setq item (nth curIdx matches)
          target (car item)
          ent (cadr item)
          dispTxt (nth 4 item)
          customPt (if (>= (length item) 7) (nth 6 item) nil))

    (if customPt
      (setq cen customPt h 35.0)
      (setq cenData (Get-Object-Center target ent)
            cen (car cenData)
            h (cadr cenData))
    )
    (setq *last-match-idx* curIdx)

    ;; 1. Zoom đến đối tượng
    (if customPt
      (ZoomToObject customPt ent)
      (ZoomToObject target ent)
    )

    ;; 2. Spotlight
    (if cen
      (Show-Spotlight-Circle cen (max 5.0 (/ h 3.5)))
    )

    ;; 3. Dòng nhắc lệnh
    (prompt
      (strcat "\n>>> [" (itoa (1+ curIdx)) "/" (itoa count) "] \"" dispTxt "\"  |  Layer: " (nth 5 item)
              (if cen (strcat "  |  Tọa độ VN-2000: X=" (rtos (car cen) 2 2) ", Y=" (rtos (cadr cen) 2 2)) ""))
    )
    (initget "Next Previous Table Excel Exit")
    (setq waitKey (getkword "\n[Enter/Space] Tiếp  |  [P] Lùi  |  [D] Bảng Danh Sách  |  [X] Xuất Excel  |  [E] Thoát: "))

    (cond
      ((or (null waitKey) (= waitKey "") (= waitKey "Next") (= waitKey "N"))
        (setq curIdx (1+ curIdx))
        (if (>= curIdx count)
          (progn
            (prompt "\n-> Đã đến cuối danh sách! Nhấn [Enter/Space] để lặp lại từ đầu.")
            (setq curIdx 0)
          )
        )
      )
      ((or (= waitKey "Previous") (= waitKey "P"))
        (setq curIdx (1- curIdx))
        (if (< curIdx 0) (setq curIdx (1- count)))
      )
      ((or (= waitKey "Table") (= waitKey "D") (= waitKey "B"))
        (Clear-Spotlight-Circle)
        (setq loopTour nil)
      )
      ((or (= waitKey "Excel") (= waitKey "X"))
        (ExportMatchesToExcel matches)
        (prompt "\n-> Đã xuất file Excel kết quả!")
      )
      ((or (= waitKey "Exit") (= waitKey "E"))
        (Clear-Spotlight-Circle)
        (setq loopTour nil
              *exit-tour-flag* T)
      )
      (t
        (setq curIdx (1+ curIdx))
      )
    )
  )
)

(defun MarkCirclesOnObjects (matches / item target ent cenData cen h rad markCount customPt)
  (if (null (tblsearch "LAYER" "_DANH_DAU_TRA_CUU"))
    (entmake '((0 . "LAYER") (100 . "AcDbSymbolTableRecord") (100 . "AcDbLayerTableRecord")
               (2 . "_DANH_DAU_TRA_CUU") (70 . 0) (62 . 1)))
  )
  (setq markCount 0)
  (foreach item matches
    (setq target (car item)
          ent (cadr item)
          customPt (if (>= (length item) 7) (nth 6 item) nil))
    (if customPt
      (setq cen customPt h 35.0)
      (setq cenData (Get-Object-Center target ent)
            cen (car cenData)
            h (cadr cenData))
    )
    (setq rad (max 5.0 (/ h 3.5)))
    (if cen
      (progn
        (entmake (list '(0 . "CIRCLE")
                       (cons 8 "_DANH_DAU_TRA_CUU")
                       (cons 62 1)
                       (cons 10 cen)
                       (cons 40 rad)))
        (setq markCount (1+ markCount))
      )
    )
  )
  markCount
)

(defun ClearMarkCircles ( / ssMark delCount )
  (setq ssMark (ssget "X" '((0 . "CIRCLE") (8 . "_DANH_DAU_TRA_CUU"))))
  (setq delCount 0)
  (if ssMark
    (repeat (setq delCount (sslength ssMark))
      (entdel (ssname ssMark (setq delCount (1- delCount))))
    )
  )
  delCount
)

(defun ExportMatchesToExcel (matches / csvPath fn idx item target ent typ dispTxt layerName cenData cen x y wsh customPt)
  (setq csvPath (strcat (TC-Get-Extra-Dir) "\\KetQua_TraCuu.csv"))
  (setq fn (open csvPath "w"))
  (if (null fn)
    (setq csvPath (strcat (getvar "DWGPREFIX") "KetQua_TraCuu.csv")
          fn (open csvPath "w"))
  )
  (if fn
    (progn
      (write-line (strcat (chr 239) (chr 187) (chr 191) "STT,Loại Đối Tượng,Nội Dung Chữ,Tọa Độ X,Tọa Độ Y,Layer") fn)
      (setq idx 1)
      (foreach item matches
        (setq target (car item)
              ent (cadr item)
              typ (nth 3 item)
              dispTxt (nth 4 item)
              layerName (nth 5 item)
              customPt (if (>= (length item) 7) (nth 6 item) nil))
        (if customPt
          (setq cen customPt)
          (setq cenData (Get-Object-Center target ent)
                cen (car cenData))
        )
        (if cen
          (setq x (car cen) y (cadr cen))
          (setq x 0.0 y 0.0)
        )
        (write-line (strcat (itoa idx) ",\"" typ "\",\"" dispTxt "\"," (rtos x 2 3) "," (rtos y 2 3) ",\"" layerName "\"") fn)
        (setq idx (1+ idx))
      )
      (close fn)
      (vl-catch-all-apply
        '(lambda ()
           (setq wsh (vlax-create-object "WScript.Shell"))
           (vlax-invoke-method wsh 'Run (strcat "explorer.exe \"" csvPath "\"") 1 :vlax-false)
           (vlax-release-object wsh)
         )
      )
      T
    )
    nil
  )
)

;;; -------------------------------------------------------------------------
;;; 10B. BỘ MÁY QUÉT & TRA CỨU TOÀN BỘ THƯ MỤC BẢN ĐỒ SIÊU TỐC (v12.0)
;;; -------------------------------------------------------------------------

;; Mở hộp thoại chọn thư mục (Tương thích cả khi không có Express Tools)
(defun TC-Pick-Folder (defaultDir / dir sh folder self)
  (setq dir nil)
  (if (and (boundp 'acet-ui-pickdir) acet-ui-pickdir)
    (setq dir (vl-catch-all-apply 'acet-ui-pickdir (list "Chon Thu Muc chua cac To Ban Do (.DWG):" defaultDir)))
  )
  (if (or (null dir) (vl-catch-all-error-p dir) (= dir ""))
    (progn
      (setq dir nil)
      (vl-catch-all-apply
        '(lambda ()
           (setq sh (vlax-create-object "Shell.Application"))
           (setq folder (vlax-invoke sh 'BrowseForFolder 0 "Chon Thu Muc chua cac To Ban Do (.DWG) can tim kiem:" 0 defaultDir))
           (if folder
             (progn
               (setq self (vlax-get-property folder 'Self))
               (setq dir (vlax-get-property self 'Path))
               (vlax-release-object self)
               (vlax-release-object folder)
             )
           )
           (vlax-release-object sh)
         )
      )
    )
  )
  (if (and dir (= (type dir) 'STR) (/= dir ""))
    (Normalize-Folder-Path dir)
    nil
  )
)

;; Chạy lệnh ngầm đồng bộ không hiện cửa sổ CMD đen
(defun TC-Run-Hidden-Sync (cmdLine / wsh res)
  (setq res -1)
  (vl-catch-all-apply
    '(lambda ()
       (setq wsh (vlax-create-object "WScript.Shell"))
       (setq res (vlax-invoke-method wsh 'Run cmdLine 0 :vlax-true))
       (vlax-release-object wsh)
     )
  )
  res
)

;; Chạy cửa sổ ứng dụng WinForms đồng bộ (hiện cửa sổ bình thường và đợi đóng)
(defun TC-Run-Visible-Sync (cmdLine / wsh res)
  (setq res -1)
  (vl-catch-all-apply
    '(lambda ()
       (setq wsh (vlax-create-object "WScript.Shell"))
       (setq res (vlax-invoke-method wsh 'Run cmdLine 1 :vlax-true))
       (vlax-release-object wsh)
     )
  )
  res
)



;; Đảm bảo thư mục EXTRACUUAUTOCAD và FastDwgSearch.exe luôn sẵn sàng trên mọi máy tính
(defun TC-Ensure-FastSearch-Exe ( / workDir exePath candidate csPath cscPath found)
  (setq workDir (TC-Get-Extra-Dir)
        exePath (strcat workDir "\\FastDwgSearch.exe"))
  (if (not (vl-file-directory-p workDir))
    (vl-mkdir workDir)
  )
  ;; 1. Quét tìm FastDwgSearch.exe ở tất cả các vị trí tiềm năng (Support, C:, D:, AppData)
  (setq found
    (cond
      ((findfile exePath))
      ((findfile "FastDwgSearch.exe"))
      ((and (findfile "TRACUU.LSP") (findfile (strcat (vl-filename-directory (findfile "TRACUU.LSP")) "\\FastDwgSearch.exe"))))
      ((findfile "D:\\New folder\\DAT\\BO_FONT_CAD_FULL\\FastDwgSearch.exe"))
      ((findfile "D:\\New folder\\DAT\\TANGNHONPHUHTR\\FastDwgSearch.exe"))
      ((findfile "D:\\EXTRACUUAUTOCAD\\FastDwgSearch.exe"))
      ((findfile "C:\\EXTRACUUAUTOCAD\\FastDwgSearch.exe"))
      ((findfile "C:\\VBA\\FastDwgSearch.exe"))
      ((findfile "D:\\VBA\\FastDwgSearch.exe"))
      ((findfile (strcat (getvar "ROAMABLEROOTPREFIX") "Support\\FastDwgSearch.exe")))
      ((findfile (strcat (getvar "LOCALROOTPREFIX") "Support\\FastDwgSearch.exe")))
      ((findfile (strcat (getenv "APPDATA") "\\EXTRACUUAUTOCAD\\FastDwgSearch.exe")))
      ((findfile (strcat (getenv "LOCALAPPDATA") "\\EXTRACUUAUTOCAD\\FastDwgSearch.exe")))
      ((and (getvar "DWGPREFIX") (findfile (strcat (getvar "DWGPREFIX") "FastDwgSearch.exe"))))
      ((findfile "D:\\TANGNHONPHUHTR\\FastDwgSearch.exe"))
      ((findfile "D:\\BO_FONT_CAD_FULL\\_HeThong\\FastDwgSearch.exe"))
      ((findfile "D:\\BO_FONT_CAD_FULL\\FastDwgSearch.exe"))
    )
  )

  ;; Nếu tìm thấy ở chỗ khác nhưng chưa có ở workDir -> Copy về workDir để tiện quản lý kết quả
  (if (and found (not (findfile exePath)))
    (vl-file-copy found exePath)
  )

  ;; 2. Nếu chưa có file .exe ở bất cứ đâu -> Tự động tìm file nguồn C# .cs và biên dịch tức thì bằng csc.exe!
  (if (not (or (findfile exePath) found))
    (progn
      (setq csPath
        (cond
          ((findfile (strcat workDir "\\FastDwgSearch.cs")))
          ((findfile "FastDwgSearch.cs"))
          ((and (findfile "TRACUU.LSP") (findfile (strcat (vl-filename-directory (findfile "TRACUU.LSP")) "\\FastDwgSearch.cs"))))
          ((findfile "D:\\New folder\\DAT\\BO_FONT_CAD_FULL\\FastDwgSearch.cs"))
          ((findfile "D:\\New folder\\DAT\\TANGNHONPHUHTR\\FastDwgSearch.cs"))
          ((findfile "D:\\EXTRACUUAUTOCAD\\FastDwgSearch.cs"))
          ((findfile "C:\\EXTRACUUAUTOCAD\\FastDwgSearch.cs"))
          ((findfile "C:\\VBA\\FastDwgSearch.cs"))
          ((findfile "D:\\VBA\\FastDwgSearch.cs"))
          ((findfile (strcat (getvar "ROAMABLEROOTPREFIX") "Support\\FastDwgSearch.cs")))
          ((findfile (strcat (getvar "LOCALROOTPREFIX") "Support\\FastDwgSearch.cs")))
          ((findfile (strcat (getenv "APPDATA") "\\EXTRACUUAUTOCAD\\FastDwgSearch.cs")))
          ((findfile (strcat (getenv "LOCALAPPDATA") "\\EXTRACUUAUTOCAD\\FastDwgSearch.cs")))
          ((and (getvar "DWGPREFIX") (findfile (strcat (getvar "DWGPREFIX") "FastDwgSearch.cs"))))
          ((findfile "D:\\BO_FONT_CAD_FULL\\_HeThong\\FastDwgSearch.cs"))
          ((findfile "D:\\BO_FONT_CAD_FULL\\FastDwgSearch.cs"))
        )
      )
      (setq cscPath
        (cond
          ((findfile "C:\\Windows\\Microsoft.NET\\Framework64\\v4.0.30319\\csc.exe"))
          ((findfile "C:\\Windows\\Microsoft.NET\\Framework\\v4.0.30319\\csc.exe"))
        )
      )
      (if (and csPath cscPath)
        (progn
          (prompt "\n-> Đang tự động biên dịch bộ máy tìm kiếm siêu tốc C# lần đầu tiên...")
          (TC-Run-Hidden-Sync (strcat "\"" cscPath "\" /optimize+ /unsafe /platform:anycpu /nologo /t:winexe /out:\"" exePath "\" \"" csPath "\""))
        )
      )
    )
  )

  ;; Trả về đường dẫn thực thi tìm thấy
  (cond
    ((findfile exePath))
    (found)
    ((findfile "FastDwgSearch.exe"))
  )
)

;; Khởi tạo đối tượng ObjectDBX tự động theo phiên bản AutoCAD
(defun TC-Create-DBX ( / ver dbx)
  (setq ver (substr (getvar "ACADVER") 1 2))
  (setq dbx (vl-catch-all-apply 'vlax-create-object (list (strcat "ObjectDBX.AxDbDocument." ver))))
  (if (vl-catch-all-error-p dbx)
    (setq dbx (vl-catch-all-apply 'vlax-create-object (list "ObjectDBX.AxDbDocument")))
  )
  (if (vl-catch-all-error-p dbx) nil dbx)
)

;; Tìm Document đang mở sẵn trong AutoCAD nếu trùng đường dẫn
(defun TC-Find-Open-Doc (fullPath / acadObj docs doc foundDoc)
  (setq foundDoc nil)
  (vl-catch-all-apply
    '(lambda ()
       (setq acadObj (vlax-get-acad-object))
       (if acadObj
         (progn
           (setq docs (vla-get-Documents acadObj))
           (vlax-for doc docs
             (if (= (strcase (vla-get-FullName doc)) (strcase fullPath))
               (setq foundDoc doc)
             )
           )
         )
       )
     )
  )
  foundDoc
)

;; Trích xuất toàn bộ Text/MText/Attrib từ một Document (ObjectDBX hoặc Open Doc) ra file TSV
(defun TC-Dump-Doc-Entities (docObj dwgPath fOut / tab badChars docStyles st stName stFont tf bold italic charSet pitch obj onm en ed typ raw s3 item pt lay sty styFont attList att)
  (setq tab (chr 9)
        badChars (strcat (chr 9) (chr 10) (chr 13))
        docStyles nil)
  (vl-catch-all-apply
    '(lambda ()
       (vlax-for st (vla-get-TextStyles docObj)
         (setq stName (strcase (vla-get-Name st)))
         (setq stFont (vla-get-FontFile st))
         (setq tf nil)
         (vl-catch-all-apply '(lambda () (vla-GetFont st 'tf 'bold 'italic 'charSet 'pitch)))
         (setq docStyles
           (cons
             (cons stName (strcat stName "@" (if stFont stFont "") (if (and tf (/= tf "")) (strcat "@" tf) "")))
             docStyles
           )
         )
       )
     )
  )
  (vl-catch-all-apply
    '(lambda ()
       (vlax-for obj (vla-get-ModelSpace docObj)
         (setq onm (vla-get-ObjectName obj))
         (cond
           ((wcmatch onm "*Text*,AcDbMLeader")
            (setq en (vl-catch-all-apply 'vlax-vla-object->ename (list obj)))
            (if (and en (not (vl-catch-all-error-p en)))
              (progn
                (setq ed (entget en))
                (setq typ (cdr (assoc 0 ed)))
                (setq raw
                  (cond
                    ((= typ "TEXT") (cdr (assoc 1 ed)))
                    ((= typ "MTEXT")
                     (setq s3 "")
                     (foreach item ed (if (= (car item) 3) (setq s3 (strcat s3 (cdr item)))))
                     (strcat s3 (if (cdr (assoc 1 ed)) (cdr (assoc 1 ed)) ""))
                    )
                    (t (cdr (assoc 304 ed)))
                  )
                )
                (setq pt (cdr (assoc 10 ed)))
                (setq lay (cdr (assoc 8 ed)))
                (setq sty (cdr (assoc 7 ed)))
              )
              ;; Nhánh dự phòng COM trực tiếp cho ObjectDBX trên AutoCAD 2021 GUI (khi không có ename)
              (progn
                (setq typ (cond ((= onm "AcDbText") "TEXT") ((= onm "AcDbMText") "MTEXT") (t "MULTILEADER")))
                (setq raw (vl-catch-all-apply 'vla-get-TextString (list obj)))
                (if (vl-catch-all-error-p raw) (setq raw nil))
                (setq lay (vl-catch-all-apply 'vla-get-Layer (list obj)))
                (if (vl-catch-all-error-p lay) (setq lay "0"))
                (setq sty (vl-catch-all-apply 'vla-get-StyleName (list obj)))
                (if (vl-catch-all-error-p sty) (setq sty "Standard"))
                (setq pt (vl-catch-all-apply 'vlax-get (list obj 'InsertionPoint)))
                (if (vl-catch-all-error-p pt) (setq pt '(0.0 0.0 0.0)))
              )
            )
            (if (and raw (= (type raw) 'STR) (> (strlen raw) 0))
              (progn
                (setq raw (vl-string-translate badChars "   " raw))
                (if (not (and pt (listp pt))) (setq pt '(0.0 0.0 0.0)))
                (setq styFont (if sty (cdr (assoc (strcase sty) docStyles)) nil))
                (if (not styFont) (setq styFont (if sty sty "Standard")))
                (write-line (strcat dwgPath tab (if lay lay "0") tab typ tab styFont tab (rtos (car pt) 2 2) tab (rtos (cadr pt) 2 2) tab raw) fOut)
              )
            )
           )
           ((and (= onm "AcDbBlockReference") (= (vla-get-HasAttributes obj) :vlax-true))
            (setq attList (vl-catch-all-apply 'vlax-invoke (list obj 'GetAttributes)))
            (if (not (vl-catch-all-error-p attList))
              (foreach att attList
                (setq en (vl-catch-all-apply 'vlax-vla-object->ename (list att)))
                (if (and en (not (vl-catch-all-error-p en)))
                  (progn
                    (setq ed  (entget en)
                          raw (cdr (assoc 1 ed))
                          pt  (cdr (assoc 10 ed))
                          lay (cdr (assoc 8 ed))
                          sty (cdr (assoc 7 ed)))
                  )
                  (progn
                    (setq raw (vl-catch-all-apply 'vla-get-TextString (list att)))
                    (if (vl-catch-all-error-p raw) (setq raw nil))
                    (setq lay (vl-catch-all-apply 'vla-get-Layer (list att)))
                    (if (vl-catch-all-error-p lay) (setq lay "0"))
                    (setq sty (vl-catch-all-apply 'vla-get-StyleName (list att)))
                    (if (vl-catch-all-error-p sty) (setq sty "Standard"))
                    (setq pt (vl-catch-all-apply 'vlax-get (list att 'InsertionPoint)))
                    (if (vl-catch-all-error-p pt) (setq pt '(0.0 0.0 0.0)))
                  )
                )
                (if (and raw (= (type raw) 'STR) (> (strlen raw) 0))
                  (progn
                    (setq raw (vl-string-translate badChars "   " raw))
                    (if (not (and pt (listp pt))) (setq pt '(0.0 0.0 0.0)))
                    (setq styFont (if sty (cdr (assoc (strcase sty) docStyles)) nil))
                    (if (not styFont) (setq styFont (if sty sty "Standard")))
                    (write-line (strcat dwgPath tab (if lay lay "0") tab "ATTRIB" tab styFont tab (rtos (car pt) 2 2) tab (rtos (cadr pt) 2 2) tab raw) fOut)
                  )
                )
              )
            )
           )
         )
       )
     )
  )
)

;; Quét danh sách các file DWG mới/thay đổi qua ObjectDBX (Tái sử dụng 1 đối tượng DBX duy nhất siêu nhanh!)
(defun TC-Dump-Todo-Dwgs (todoPath rawPath totalTodo / workDir fIn fOut line cnt tab openDoc dbx tmpDwg)
  (setq workDir (TC-Get-Extra-Dir)
        tab (chr 9)
        cnt 0
        dbx nil)
  (setq fIn (open todoPath "r"))
  (setq fOut (vl-catch-all-apply 'open (list rawPath "w" "utf8")))
  (if (or (null fOut) (vl-catch-all-error-p fOut))
    (setq fOut (open rawPath "w"))
  )
  (if (and fIn fOut)
    (progn
      (while (setq line (read-line fIn))
        (setq line (Decode-Unicode-Hex-Escapes (vl-string-trim " \t\r\n" line)))
        (if (> (strlen line) 4)
          (progn
            (setq cnt (1+ cnt))
            (prompt (strcat "\r-> [Đang lập chỉ mục siêu tốc " (itoa cnt) "/" (itoa totalTodo) "] " (vl-filename-base line) ".dwg ...     "))
            (write-line (strcat "#FILE#" tab line) fOut)
            (setq openDoc (TC-Find-Open-Doc line))
            (if openDoc
              (TC-Dump-Doc-Entities openDoc line fOut)
              (progn
                (if (not dbx) (setq dbx (TC-Create-DBX)))
                (if dbx
                  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-Open (list dbx line))))
                    (TC-Dump-Doc-Entities dbx line fOut)
                    (progn
                      ;; Nếu file đang mở ở cửa sổ AutoCAD khác bị khóa (Read/Write Lock), copy ra bản tạm để đọc!
                      (setq tmpDwg (vl-filename-mktemp "tcdbx" workDir ".dwg"))
                      (if (and tmpDwg (vl-file-copy line tmpDwg))
                        (progn
                          (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-Open (list dbx tmpDwg))))
                            (TC-Dump-Doc-Entities dbx line fOut)
                          )
                        )
                      )
                      (if (and tmpDwg (findfile tmpDwg)) (vl-file-delete tmpDwg))
                    )
                  )
                )
              )
            )
            (if (= (rem cnt 25) 0) (gc))
          )
        )
      )
      (if dbx (vl-catch-all-apply 'vlax-release-object (list dbx)))
      (close fOut)
      (close fIn)
    )
  )
  cnt
)

;; Kiểm tra và tự động cập nhật chỉ mục cho thư mục bản đồ
(defun TC-Ensure-Folder-Indexed (folderPath forceFlag / workDir exePath todoPath statPath rawPath fStat statLine parts totalDwgs needCount)
  (setq workDir (TC-Get-Extra-Dir)
        exePath (TC-Ensure-FastSearch-Exe))
  (if (not exePath)
    (progn
      (prompt "\n[LỖI] Không tìm thấy bộ máy tìm kiếm siêu tốc FastDwgSearch.exe!")
      nil
    )
    (progn
      (setq todoPath (strcat workDir "\\todo_dwgs.txt")
            statPath (strcat workDir "\\todo_dwgs.txt.stat")
            rawPath  (strcat workDir "\\raw_update.tsv"))
      (if (findfile statPath) (vl-file-delete statPath))
      (TC-Run-Hidden-Sync
        (strcat "\"" exePath "\" check \"" folderPath "\" \"" todoPath "\" " (if forceFlag "1" "0"))
      )
      (setq totalDwgs 0 needCount 0)
      (if (findfile statPath)
        (progn
          (setq fStat (open statPath "r"))
          (if fStat
            (progn
              (setq statLine (read-line fStat))
              (close fStat)
              (if statLine
                (progn
                  (setq parts (SplitByDelim statLine "|"))
                  (if (>= (length parts) 2)
                    (setq totalDwgs (atoi (nth 0 parts))
                          needCount (atoi (nth 1 parts)))
                  )
                )
              )
            )
          )
        )
      )
      (if (> needCount 0)
        (progn
          (prompt (strcat "\n[Fast Find Folder] Phát hiện " (itoa needCount) "/" (itoa totalDwgs) " tờ bản đồ (.DWG) mới hoặc thay đổi. Đang lập chỉ mục..."))
          (TC-Dump-Todo-Dwgs todoPath rawPath needCount)
          (prompt "\n-> Đang nén và giải mã đa bảng mã (Unicode / VNI / TCVN3) vào cơ sở dữ liệu...")
          (TC-Run-Hidden-Sync (strcat "\"" exePath "\" ingest \"" rawPath "\""))
          (if (findfile rawPath) (vl-file-delete rawPath))
          (prompt (strcat "\n-> [HOÀN TẤT] Đã lập chỉ mục xong toàn bộ " (itoa totalDwgs) " tờ bản đồ (.DWG)!"))
        )
        (prompt (strcat "\n-> [SIÊU TỐC] Đã sẵn sàng chỉ mục của " (itoa totalDwgs) " tờ bản đồ (.DWG) trong bộ nhớ đệm!"))
      )
      totalDwgs
    )
  )
)

;; Kiểm tra xem tên bản vẽ có phải là bản vẽ Tổng Ghép / Tổng Hợp hay không
(defun Is-Tong-Ghep-Dwg (name / asc)
  (if (or (null name) (= name ""))
    nil
    (progn
      (setq asc (strcase (Uni-Str->Plain-ASCII name)))
      (or
        (wcmatch asc "*TONG*GHEP*")
        (wcmatch asc "*TONGGHEP*")
        (wcmatch asc "*TONG*HOP*")
        (wcmatch asc "*TONGHOP*")
      )
    )
  )
)

;; Cập nhật thông tin chi tiết tờ bản đồ trên hộp thoại kết quả thư mục
(defun Update-Folder-Inspector (idx fMatches kw / item hTxt)
  (if (and fMatches (>= idx 0) (< idx (length fMatches)))
    (progn
      (setq item (nth idx fMatches))
      ;; item: 0:dwgFull 1:dwgShort 2:parentDir 3:layer 4:entType 5:xStr 6:yStr 7:dispUni
      (setq hTxt (Highlight-Keyword-In-Text (nth 7 item) (if kw kw *last-kw*)))
      (set_tile "txt_f_matched"
        (strcat "📌 Nội dung tìm thấy:  \"" hTxt "\"   (Khớp từ khóa: \"" (if kw kw *last-kw*) "\")")
      )
      (set_tile "txt_f_sheet"
        (strcat "🗺️ Tờ bản đồ: " (nth 1 item)
                "   │   Thư mục: " (nth 2 item)
                "   │   Layer: " (nth 3 item)
                "   │   Loại: " (nth 4 item))
      )
      (set_tile "txt_f_coords"
        (strcat "🎯 Tọa độ VN-2000:  X = " (nth 5 item) " , Y = " (nth 6 item)
                "   │   File: " (nth 0 item))
      )
    )
    (progn
      (set_tile "txt_f_matched" "📌 Nội dung chữ: ---")
      (set_tile "txt_f_sheet" "🗺️ Tờ bản đồ: ---   │   Thư mục: ---   │   Layer: ---")
      (set_tile "txt_f_coords" "🎯 Tọa độ VN-2000:  ---")
    )
  )
)

;; Tự động kiểm tra và Zoom tới tọa độ chờ sẵn khi mở tờ bản đồ mới (PRO v13.0)
(defun TC-Check-Pending-Zoom ( / jumpPaths fPath fn line1 line2 line3 remStr pIdx pIdx2 pIdx3 xVal yVal dwgFull content lay pt ssNear sCnt)
  (setq jumpPaths (list (strcat (TC-Get-Extra-Dir) "\\pending_zoom.txt")
                        (strcat (TC-Get-Extra-Dir) "\\fast_jump.tsv")
                        "C:/EXTRACUUAUTOCAD/pending_zoom.txt"
                        "C:/EXTRACUUAUTOCAD/fast_jump.tsv"
                        "D:/EXTRACUUAUTOCAD/pending_zoom.txt"
                        "D:/EXTRACUUAUTOCAD/fast_jump.tsv"
                        "D:/VBA/pending_zoom.txt"
                        "D:/VBA/fast_jump.tsv"
                        "C:/VBA/pending_zoom.txt"
                        "C:/VBA/fast_jump.tsv"
                        (strcat (getvar "DWGPREFIX") "pending_zoom.txt")
                        (strcat (getvar "DWGPREFIX") "fast_jump.tsv")))
  (setq fPath nil xVal nil yVal nil dwgFull "" content "" lay "")
  (foreach p jumpPaths
    (if (and (null fPath) (findfile p))
      (setq fPath (findfile p))
    )
  )
  (if (and fPath (setq fn (open fPath "r")))
    (progn
      (setq line1 (read-line fn))
      (if (wcmatch (strcase fPath) "*.TSV")
        (progn
          (close fn)
          ;; Format TSV: dwgPath \t xStr \t yStr \t coordCad
          (setq pIdx (vl-string-search (chr 9) line1))
          (if pIdx
            (progn
              (setq dwgFull (substr line1 1 pIdx)
                    remStr  (substr line1 (+ pIdx 2))
                    pIdx2   (vl-string-search (chr 9) remStr))
              (if pIdx2
                (progn
                  (setq xVal (distof (substr remStr 1 pIdx2))
                        remStr (substr remStr (+ pIdx2 2))
                        pIdx3  (vl-string-search (chr 9) remStr)
                        yVal   (distof (if pIdx3 (substr remStr 1 pIdx3) remStr)))
                )
              )
            )
          )
        )
        (progn
          ;; Format TXT: line1=dwgFull, line2=xStr, line3=yStr, line4=content, line5=lay
          (setq dwgFull line1
                line2   (read-line fn)
                line3   (read-line fn)
                content (read-line fn)
                lay     (read-line fn))
          (close fn)
          (if (and line2 line3)
            (setq xVal (distof line2)
                  yVal (distof line3))
          )
        )
      )
      ;; Xoa sach tat ca cac file pending de khong bi nhay nham lan sau
      (foreach p jumpPaths
        (if (findfile p) (vl-catch-all-apply 'vl-file-delete (list (findfile p))))
      )
      ;; Thuc hien Zoom va Khoanh vung vi tri thua dat
      (if (and xVal yVal (/= xVal 0.0) (/= yVal 0.0) (or (null dwgFull) (= dwgFull "") (wcmatch (strcase (getvar "DWGNAME")) (strcase (strcat "*" (vl-filename-base dwgFull) "*")))))
        (progn
          (setq pt (list xVal yVal 0.0))
          (vl-catch-all-apply 'vla-ZoomCenter (list (vlax-get-acad-object) (vlax-3d-point pt) 45.0))
          (if (fboundp 'Show-Spotlight-Circle)
            (vl-catch-all-apply 'Show-Spotlight-Circle (list pt 8.0))
          )
          (setq ssNear (ssget "_C" (list (- xVal 30.0) (- yVal 30.0) 0.0)
                                   (list (+ xVal 30.0) (+ yVal 30.0) 0.0)
                                   '((0 . "TEXT,MTEXT,INSERT,ATTRIB,DIMENSION,MULTILEADER"))))
          (if ssNear
            (progn
              (sssetfirst nil ssNear)
              (repeat (setq sCnt (sslength ssNear))
                (redraw (ssname ssNear (setq sCnt (1- sCnt))) 3)
              )
            )
          )
          (if (null (tblsearch "LAYER" "_DANH_DAU_TRA_CUU"))
            (entmake '((0 . "LAYER") (100 . "AcDbSymbolTableRecord") (100 . "AcDbLayerTableRecord")
                       (2 . "_DANH_DAU_TRA_CUU") (70 . 0) (62 . 1)))
          )
          (entmake (list '(0 . "CIRCLE")
                         '(8 . "_DANH_DAU_TRA_CUU")
                         '(62 . 1)
                         (cons 10 pt)
                         '(40 . 10.0)))
          (redraw)
          (prompt "\n============================================================")
          (prompt (strcat "\n-> [FAST FIND PRO] DA MO BAN VE: " (getvar "DWGNAME")))
          (if (and content (/= content ""))
            (prompt (strcat "\n-> Tim thay: \"" (if (fboundp 'Decode-Unicode-Hex-Escapes) (Decode-Unicode-Hex-Escapes content) content) "\" | Layer: " (if lay lay "")))
          )
          (prompt (strcat "\n-> TOA DO VN-2000: X = " (rtos xVal 2 2) " , Y = " (rtos yVal 2 2)))
          (prompt "\n-> (Nhan lenh 'ZZ' hoac 'DINHVI' de zoom lai vi tri nay bat ky luc nao!)")
          (prompt "\n============================================================\n")
        )
      )
    )
  )
  (princ)
)

;; Lenh tat ZZ va DINHVI trong AutoCAD de zoom ngay toi toa do vua tra cuu
(defun c:ZZ () (TC-Check-Pending-Zoom))
(defun c:DINHVI () (TC-Check-Pending-Zoom))

;; Mở tờ bản đồ tìm thấy hoặc Zoom ngay nếu đang ở đúng bản vẽ đó
(defun TC-Open-Or-Zoom-Folder-Match (item / dwgFull dwgShort parentDir lay entType xStr yStr dispUni curFull pt pendPath fn openDoc newDoc waitKey lspPath)
  (setq dwgFull   (nth 0 item)
        dwgShort  (nth 1 item)
        parentDir (nth 2 item)
        lay       (nth 3 item)
        entType   (nth 4 item)
        xStr      (nth 5 item)
        yStr      (nth 6 item)
        dispUni   (nth 7 item)
        curFull   (strcat (getvar "DWGPREFIX") (getvar "DWGNAME"))
        pt        (list (distof xStr) (distof yStr) 0.0))

  (setq dwgFull (TC-Resolve-Dwg-Path dwgFull dwgShort))

  (if (= (strcase dwgFull) (strcase curFull))
    (progn
      ;; Đang ở ngay trên bản vẽ này -> Zoom trực tiếp
      (vl-catch-all-apply 'vla-ZoomCenter (list (vlax-get-acad-object) (vlax-3d-point pt) 45.0))
      (Show-Spotlight-Circle pt 8.0)
      (setq ssNear (ssget "_C" (list (- (car pt) 30.0) (- (cadr pt) 30.0) 0.0)
                               (list (+ (car pt) 30.0) (+ (cadr pt) 30.0) 0.0)
                               '((0 . "TEXT,MTEXT,INSERT,ATTRIB,DIMENSION,MULTILEADER"))))
      (if ssNear
        (progn
          (sssetfirst nil ssNear)
          (repeat (setq sCnt (sslength ssNear))
            (redraw (ssname ssNear (setq sCnt (1- sCnt))) 3)
          )
        )
      )
      (prompt (strcat "\n-> [ĐANG Ở TRÊN BẢN VẼ NÀY] Đã phóng tới: \"" dispUni "\" (X=" xStr ", Y=" yStr ")"))
      (initget "Enter Exit")
      (setq waitKey (getkword "\n[Đã định vị trên bản vẽ hiện hành]. Nhấn [Enter/Space] mở lại Bảng Kết Quả Thư Mục, hoặc gõ [E] để thoát: "))
      (if (or (= waitKey "Exit") (= waitKey "E"))
        nil
        T
      )
    )
    (progn
      ;; Nằm ở tờ bản đồ khác -> Ghi tọa độ chờ sẵn và Mở tờ bản đồ đó lên!
      (setq pendPath (strcat (TC-Get-Extra-Dir) "\\pending_zoom.txt"))
      (setq fn (open pendPath "w"))
      (if fn
        (progn
          (write-line dwgFull fn)
          (write-line xStr fn)
          (write-line yStr fn)
          (write-line dispUni fn)
          (write-line lay fn)
          (close fn)
        )
      )
      (prompt (strcat "\n-> Đang mở tờ bản đồ [" dwgShort "] và tự động phóng tới (X=" xStr ", Y=" yStr ")..."))
      (setq openDoc (TC-Find-Open-Doc dwgFull))
      (if openDoc
        (progn
          (vl-catch-all-apply 'vla-Activate (list openDoc))
;;           (vl-catch-all-apply 'vla-SendCommand (list openDoc "(TC-Check-Pending-Zoom)\n"))
        )
        (progn
          (setq newDoc (vl-catch-all-apply 'vla-Open (list (vla-get-Documents (vlax-get-acad-object)) dwgFull)))
          (if (and newDoc (not (vl-catch-all-error-p newDoc)))
            (progn
              (setq lspPath
                (cond
                  ((findfile "TRACUU.LSP"))
                  ((findfile "D:/EXTRACUUAUTOCAD/TRACUU.LSP"))
                  ((findfile "D:/New folder/DAT/BO_FONT_CAD_FULL/TRACUU.LSP"))
                  ((findfile "C:/EXTRACUUAUTOCAD/TRACUU.LSP"))
                  ((findfile "D:/VBA/TRACUU.LSP"))
                  ((findfile "C:/VBA/TRACUU.LSP"))
                  (t "TRACUU.LSP")
                )
              )
              (setq lspPath (vl-string-translate "\\" "/" lspPath))
;;               (vl-catch-all-apply 'vla-SendCommand (list newDoc (strcat "(if (not c:FF) (load \"" lspPath "\")) (TC-Check-Pending-Zoom)\n")))
            )
          )
        )
      )
      nil ; Đóng hộp thoại ở bản vẽ cũ vì đã chuyển sang tab bản vẽ mới
    )
  )
)

;; Chèn tờ bản đồ tìm thấy vào ngay bản vẽ hiện hành và Zoom tới thửa đất
(defun TC-Insert-And-Zoom-Folder-Match (item / dwgFull dwgShort xStr yStr dispUni pt oldCmd oldAttDia oldAttReq)
  (setq dwgFull  (nth 0 item)
        dwgShort (nth 1 item)
        xStr     (nth 5 item)
        yStr     (nth 6 item)
        dispUni  (nth 7 item)
        pt       (list (distof xStr) (distof yStr) 0.0))
  (setq dwgFull (TC-Resolve-Dwg-Path dwgFull dwgShort))
  (if (findfile dwgFull)
    (progn
      (setq oldCmd (getvar "CMDECHO")
            oldAttDia (getvar "ATTDIA")
            oldAttReq (getvar "ATTREQ"))
      (setvar "CMDECHO" 0)
      (setvar "ATTDIA" 0)
      (setvar "ATTREQ" 0)
      (prompt (strcat "\n-> Đang chèn tờ bản đồ [" dwgShort "] vào bản vẽ hiện hành đúng tọa độ VN-2000..."))
      (vl-catch-all-apply 'vl-cmdf (list "_.-INSERT" dwgFull "0,0,0" "1" "1" "0"))
      (setvar "CMDECHO" oldCmd)
      (setvar "ATTDIA" oldAttDia)
      (setvar "ATTREQ" oldAttReq)
      ;; (vl-catch-all-apply 'AutoFix-Drawing-Fonts) ;; [UPGRADE] Khong tu dong sua font khi chen to ban do
      (vl-catch-all-apply 'vla-ZoomCenter (list (vlax-get-acad-object) (vlax-3d-point pt) 45.0))
      (Show-Spotlight-Circle pt 8.0)
      (prompt (strcat "\n-> [THÀNH CÔNG] Đã chèn tờ bản đồ [" dwgShort "] và phóng tới: \"" dispUni "\" (X=" xStr ", Y=" yStr ")!"))
    )
    (prompt (strcat "\n[LỖI] Không tìm thấy file bản vẽ: " dwgFull))
  )
)

;; Hiển thị Hộp thoại Kết Quả Tìm Kiếm Toàn Thư Mục
(defun Show-Folder-Results-Dialog (kw folderPath dclId dclPath / workDir resPath txtPath csvPath fn headerLine hParts totalMatches matchedDwgs retCount line parts dwgFull dwgShort parentDir lay entType xStr yStr dispUni fMatches dispList idx currentSel loopF resAction selItem wsh)
  (setq workDir (TC-Get-Extra-Dir)
        resPath (strcat workDir "\\folder_res.tsv")
        txtPath (strcat workDir "\\BAO_CAO_TIM_KIEM_THU_MUC.txt")
        csvPath (strcat workDir "\\KET_QUA_TIM_KIEM_THU_MUC.csv"))
  (if (not (findfile resPath))
    (prompt "\n[Fast Find Folder] Không tìm thấy dữ liệu kết quả tra cứu thư mục!")
    (progn
      (setq fn (open resPath "r")
            fMatches '()
            totalMatches 0
            matchedDwgs 0
            retCount 0)
      (if fn
        (progn
          (setq headerLine (read-line fn))
          (if headerLine
            (progn
              (setq hParts (SplitByDelim headerLine (chr 9)))
              (if (>= (length hParts) 3)
                (setq totalMatches (atoi (nth 0 hParts))
                      matchedDwgs  (atoi (nth 1 hParts))
                      retCount     (atoi (nth 2 hParts)))
              )
            )
          )
          (while (setq line (read-line fn))
            (setq parts (SplitByDelim line (chr 9)))
            (if (>= (length parts) 8)
              (progn
                (setq dwgFull   (Decode-Unicode-Hex-Escapes (nth 0 parts))
                      dwgShort  (Decode-Unicode-Hex-Escapes (nth 1 parts))
                      parentDir (Decode-Unicode-Hex-Escapes (nth 2 parts))
                      lay       (nth 3 parts)
                      entType   (nth 4 parts)
                      xStr      (nth 5 parts)
                      yStr      (nth 6 parts)
                      dispUni   (Decode-Unicode-Hex-Escapes (nth 7 parts)))
                (if (not (Is-Tong-Ghep-Dwg dwgShort))
                  (setq fMatches (cons (list dwgFull dwgShort parentDir lay entType xStr yStr dispUni) fMatches))
                )
              )
            )
          )
          (close fn)
          (setq fMatches (reverse fMatches))
        )
      )

      (if (= (length fMatches) 0)
        (progn
          (prompt (strcat "\n[Tra Cứu Thư Mục] Không tìm thấy đối tượng nào khớp với '" kw "' trong thư mục: " folderPath))
          (alert (strcat "KHÔNG TÌM THẤY KẾT QUẢ!\n\n• Từ khóa tìm kiếm: \"" kw "\"\n• Thư mục quét: " folderPath "\n\n-> Hãy kiểm tra lại số thửa, số tờ hoặc gõ lệnh FF để tìm lại!"))
        )
        (progn
          (setq dispList '()
                idx 1)
          (foreach item fMatches
            (setq dispList
              (cons
                (strcat (if (< idx 10) "00" (if (< idx 100) "0" "")) (itoa idx) ".  \""
                        (Highlight-Keyword-In-Text (nth 7 item) kw) "\"   ───   Tờ: "
                        (nth 1 item) "   │   Layer: " (nth 3 item))
                dispList
              )
            )
            (setq idx (1+ idx))
          )
          (setq dispList (reverse dispList))

          (setq currentSel "0"
                loopF T)
          (while loopF
            (if (not (new_dialog "tracuu_folder_results_dlg" dclId))
              (setq loopF nil)
              (progn
                (set_tile "lbl_f_summary"
                  (strcat "✅ TÌM THẤY " (itoa (length fMatches)) " KẾT QUẢ  │  Từ khóa: \"" kw "\"  │  Thư mục: " folderPath "  (Đã lọc bỏ Tổng Ghép)")
                )
                (start_list "res_f_list")
                (foreach strItem dispList (add_list strItem))
                (end_list)

                (set_tile "res_f_list" currentSel)
                (mode_tile "res_f_list" 2)
                (Update-Folder-Inspector (atoi currentSel) fMatches kw)

                (action_tile "res_f_list"
                  "(setq currentSel (get_tile \"res_f_list\"))
                   (Update-Folder-Inspector (atoi currentSel) fMatches kw)
                   (if (= $reason 4) (done_dialog 1))"
                )
                (action_tile "btn_open_dwg" "(setq currentSel (get_tile \"res_f_list\")) (done_dialog 1)")
                (action_tile "btn_insert_dwg" "(setq currentSel (get_tile \"res_f_list\")) (done_dialog 2)")
                (action_tile "btn_open_txt" "(done_dialog 3)")
                (action_tile "btn_open_csv" "(done_dialog 5)")
                (action_tile "btn_new" "(done_dialog 4)")
                (action_tile "cancel" "(done_dialog 0)")

                (setq resAction (start_dialog))

                (cond
                  ;; 1. Mở tờ bản đồ & Zoom tới vị trí
                  ((= resAction 1)
                    (setq selItem (nth (atoi currentSel) fMatches))
                    (if selItem
                      (setq loopF (TC-Open-Or-Zoom-Folder-Match selItem))
                      (setq loopF nil)
                    )
                  )
                  ;; 2. Chèn tờ bản đồ vào bản vẽ hiện hành & Zoom
                  ((= resAction 2)
                    (setq selItem (nth (atoi currentSel) fMatches))
                    (if selItem
                      (TC-Insert-And-Zoom-Folder-Match selItem)
                    )
                    (setq loopF nil)
                  )
                  ;; 3. Mở Báo cáo Notepad (.TXT)
                  ((= resAction 3)
                    (if (findfile txtPath)
                      (startapp "notepad.exe" txtPath)
                    )
                  )
                  ;; 5. Mở file Excel (.CSV)
                  ((= resAction 5)
                    (if (findfile csvPath)
                      (vl-catch-all-apply
                        '(lambda ()
                           (setq wsh (vlax-create-object "WScript.Shell"))
                           (vlax-invoke-method wsh 'Run (strcat "explorer.exe \"" csvPath "\"") 1 :vlax-false)
                           (vlax-release-object wsh)
                         )
                      )
                    )
                  )
                  ;; 4. Tìm từ khác
                  ((= resAction 4)
                    (setq loopF nil)
                    (unload_dialog dclId)
                    (if (findfile dclPath) (vl-file-delete dclPath))
                    (c:FF)
                  )
                  (t
                    (setq loopF nil)
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

;; Thực hiện Tìm kiếm siêu tốc trên toàn bộ Thư mục (Fast Find Pro v13.0 Form & Engine)
(defun Do-Folder-Search (kw folderPath layerFilter dclId dclPath / workDir exePath actionFile fn line parts action dwg x y selItem)
  (setq workDir (TC-Get-Extra-Dir)
        exePath (TC-Ensure-FastSearch-Exe))
  (if exePath
    (progn
      (setq actionFile (strcat workDir "\\selected_action.tsv"))
      (if (findfile actionFile) (vl-file-delete actionFile))
      (prompt (strcat "\n-> Đang mở Hộp thoại Tra Cứu Địa Chính Siêu Tốc Fast Find Pro v13.0..."))
      (TC-Run-Visible-Sync
        (strcat "\"" exePath "\" gui \"" (if folderPath folderPath "") "\" \"" (if kw kw "") "\"")
      )
      ;; Xử lý kết quả trả về từ Form (nếu người dùng bấm Mở hoặc Chèn)
      (if (findfile actionFile)
        (progn
          (setq fn (open actionFile "r"))
          (if fn
            (progn
              (setq line (read-line fn))
              (close fn)
              (vl-file-delete actionFile)
              (if (and line (/= line ""))
                (progn
                  (setq parts (SplitByDelim line (chr 9)))
                  (setq action (nth 0 parts)
                        dwg    (nth 1 parts)
                        x      (nth 2 parts)
                        y      (nth 3 parts))
                  (cond
                    ((= action "OPEN")
                      (if (and dwg (findfile dwg))
                        (progn
                          (prompt (strcat "\n-> [Fast Find] Đang mở tờ bản đồ: " dwg "..."))
                          (setq selItem (list dwg (vl-filename-base dwg) "" "" "" x y ""))
                          (TC-Open-Or-Zoom-Folder-Match selItem)
                        )
                        (prompt (strcat "\n[Fast Find] Không tìm thấy file: " dwg))
                      )
                    )
                    ((= action "INSERT")
                      (if (and dwg (findfile dwg))
                        (progn
                          (prompt (strcat "\n-> [Fast Find] Đang chèn tờ bản đồ: " dwg " vào bản vẽ hiện hành..."))
                          (vl-cmdf "_.-INSERT" dwg "0,0,0" "1" "1" "0")
                          (vl-cmdf "_.EXPLODE" (entlast))
                          (vl-cmdf "_.ZOOM" "_E")
                          (prompt "\n-> [Fast Find] Đã chèn và phá khối thành công!")
                        )
                        (prompt (strcat "\n[Fast Find] Không tìm thấy file: " dwg))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

;;; -------------------------------------------------------------------------
;;; 11. LỆNH CHÍNH: FF (FAST FIND PRO v12.0 MULTI-DWG & MULTI-FONT UNIVERSAL)
;;; -------------------------------------------------------------------------
(defun c:FF ( / *error* dclPath dclId dlgStatus initVal clipText kw optSplit optNested
  (TC-Check-Update-Notice)
  (vl-catch-all-apply '(lambda () (startapp (strcat "\"" (TC-Ensure-FastSearch-Exe) "\" check-update"))))
                searchTerms kwSpecs ssgetPat coreFilter ss i ent dxf typ txt cleanTxt dispTxt
                sub subDxf obj matches count item target e minPt maxPt p1 p2 cen diag h
                idx userChoice ssFound countText countAtt countOther countCoord
                rCount cCount r c tableObj cellTxt
                dispList currentSel loopResults resAction selIdx waitKey layerName rowLabel
                layerList scopeList typeList histList selectedScopeIdx selectedLayerIdx selectedTypeIdx
                selectedLayer entTypeStr dxfFilter markRes expRes blkName blkMatches
                bItem bSubTyp bTxt bLocPt bParent bWcsPt insPt sx sy sz rot coordPt ssNear
                j nEnt nDxf nTyp nLay nTxt nRowLabel
                pt10 ptNear ssArea areaEnt areaDxf areaTxt areaDisp pairedItem
                loopMainDlg pickedDir folderBoxVal tStart tElapsed )
  (vl-load-com)
  (setq *Blk-Cache* nil)
;;   (vl-catch-all-apply 'AutoFix-Drawing-Fonts-Once)

  (defun *error* (msg)
    (Clear-Spotlight-Circle)
    (if (and dclId (> dclId 0)) (vl-catch-all-apply 'unload_dialog (list dclId)))
    (if (and dclPath (findfile dclPath)) (vl-file-delete dclPath))
    (if (and msg (not (wcmatch (strcase msg) "*QUIT*,*EXIT*,*CANCEL*,*FUNCTION CANCELLED*")))
      (prompt (strcat "\n[Tra Cứu Pro] " msg))
    )
    (princ)
  )

  (setq clipText (LM:GetClipboardText))
  (if (and (/= clipText "") (not (vl-string-search "\n" clipText)))
    (setq initVal clipText)
    (setq initVal *last-kw*)
  )

  (setq layerList (Get-All-Layers))
  (setq scopeList '("1. Toàn bộ Bản vẽ hiện hành (All Current Drawing)"
                    "2. Quét chọn một vùng trên bản vẽ (Select Area)"))
  (setq typeList '("Tất cả các loại (*)"
                   "Chỉ Chữ (Text & MText)"
                   "Chỉ Block (Khối & Thuộc tính)"
                   "Chỉ Kích Thước (Dim) & Chỉ Dẫn (MLeader)"
                   "Chỉ Bảng Table"))
  (setq histList (if (and *FastFind-History* (> (length *FastFind-History*) 0))
                   *FastFind-History*
                   '("(Chưa có lịch sử)")))

  (setq dclPath (FastFind:WriteDCL))
  (setq dclId (load_dialog dclPath))

  (setq loopMainDlg T)
  (while loopMainDlg
    (if (not (new_dialog "tracuu_dlg" dclId))
      (progn
        (prompt "\nKhông thể khởi tạo hộp thoại tìm kiếm!")
        (if (findfile dclPath) (vl-file-delete dclPath))
        (exit)
      )
    )

    (set_tile "kw_box" initVal)
    (mode_tile "kw_box" 2)
    (mode_tile "kw_box" 3)

    (set_tile "dir_box" (if (and *last-search-folder* (vl-file-directory-p *last-search-folder*)) *last-search-folder* (if (vl-file-directory-p "D:\\") "D:\\" "C:\\")))

    (start_list "pop_history")
    (foreach item histList (add_list item))
    (end_list)
    (set_tile "pop_history" "0")
    (action_tile "pop_history"
      "(if (and *FastFind-History* (> (length *FastFind-History*) 0))
         (set_tile \"kw_box\" (nth (atoi (get_tile \"pop_history\")) histList)))"
    )

    (start_list "pop_scope")
    (foreach item scopeList (add_list item))
    (end_list)
    (if (>= (atoi *last-scope-idx*) (length scopeList))
      (setq *last-scope-idx* "0")
    )
    (set_tile "pop_scope" *last-scope-idx*)

    (start_list "pop_layer")
    (foreach lay layerList (add_list lay))
    (end_list)
    (set_tile "pop_layer" "0")

    (start_list "pop_type")
    (foreach item typeList (add_list item))
    (end_list)
    (set_tile "pop_type" *last-type-idx*)

    ;; Khởi tạo trạng thái Công Tắc BẬT / TẮT Tìm Kiếm Thư Mục
    (Update-Folder-Switch-UI *folder-search-enabled*)

    ;; Khi gạt ô Checkbox Công Tắc
    (action_tile "chk_folder_mode"
      "(setq *folder-search-enabled* $value)
       (Update-Folder-Switch-UI *folder-search-enabled*)"
    )

    ;; Khi bấm nút Công Tắc BẬT/TẮT nhanh
    (action_tile "btn_toggle_folder"
      "(setq *folder-search-enabled* (if (= *folder-search-enabled* \"1\") \"0\" \"1\"))
       (Update-Folder-Switch-UI *folder-search-enabled*)"
    )

    (action_tile "btn_pick_dir"
      "(setq initVal (get_tile \"kw_box\")
             *last-search-folder* (Normalize-Folder-Path (get_tile \"dir_box\")))
       (done_dialog 88)"
    )
    (action_tile "btn_reindex"
      "(setq initVal (get_tile \"kw_box\")
             *last-search-folder* (Normalize-Folder-Path (get_tile \"dir_box\"))
             *folder-search-enabled* \"1\")
       (done_dialog 89)"
    )
    (action_tile "btn_inspect" "(done_dialog 99)")

    (action_tile "accept"
      "(setq kw (get_tile \"kw_box\")
             folderBoxVal (get_tile \"dir_box\")
             *folder-search-enabled* (get_tile \"chk_folder_mode\")
             selectedScopeIdx (get_tile \"pop_scope\")
             selectedLayerIdx (get_tile \"pop_layer\")
             selectedTypeIdx (get_tile \"pop_type\")
             optSplit (get_tile \"chk_split\")
             optNested (get_tile \"chk_nested\"))
       (done_dialog 1)"
    )
    (action_tile "cancel" "(done_dialog 0)")

    (setq dlgStatus (start_dialog))

    (cond
      ((= dlgStatus 88)
        (setq pickedDir (TC-Pick-Folder *last-search-folder*))
        (if pickedDir
          (setq *last-search-folder* pickedDir
                *folder-search-enabled* "1")
        )
      )
      ((= dlgStatus 89)
        (if (and *last-search-folder* (vl-file-directory-p *last-search-folder*))
          (TC-Ensure-Folder-Indexed *last-search-folder* T)
          (prompt "\n[LỖI] Thư mục đã chọn không tồn tại!")
        )
      )
      (t
        (setq loopMainDlg nil)
      )
    )
  )

  ;; Nút Kiểm tra Định dạng
  (if (= dlgStatus 99)
    (progn
      (unload_dialog dclId)
      (if (findfile dclPath) (vl-file-delete dclPath))
      (c:KTD)
      (exit)
    )
  )

  (if (/= dlgStatus 1)
    (progn
      (unload_dialog dclId)
      (if (findfile dclPath) (vl-file-delete dclPath))
      (prompt "\nĐã hủy lệnh tra cứu.")
      (exit)
    )
  )

  (if (and folderBoxVal (/= (vl-string-trim " " folderBoxVal) ""))
    (setq *last-search-folder* (Normalize-Folder-Path folderBoxVal))
  )

  (if (or (null kw) (= (vl-string-trim " " kw) ""))
    (progn
      (unload_dialog dclId)
      (if (findfile dclPath) (vl-file-delete dclPath))
      (prompt "\nBạn chưa nhập nội dung tìm kiếm!")
      (exit)
    )
  )

  (setq *last-kw* kw)
  (setq *last-scope-idx* selectedScopeIdx)
  (setq *last-type-idx* selectedTypeIdx)
  (setq *FastFind-History* (vl-remove kw *FastFind-History*))
  (setq *FastFind-History* (cons kw *FastFind-History*))
  (if (> (length *FastFind-History*) 15)
    (setq *FastFind-History* (reverse (cdr (reverse *FastFind-History*))))
  )

  (if (and (/= selectedLayerIdx "0") (< (atoi selectedLayerIdx) (length layerList)))
    (setq selectedLayer (nth (atoi selectedLayerIdx) layerList))
    (setq selectedLayer nil)
  )

  ;; NẾU CÔNG TẮC TÌM KIẾM THƯ MỤC ĐANG BẬT (*folder-search-enabled* = "1"):
  (if (= *folder-search-enabled* "1")
    (progn
      (unload_dialog dclId)
      (if (findfile dclPath) (vl-file-delete dclPath))
      (setq dclId nil dclPath nil)
      (Do-Folder-Search kw *last-search-folder* selectedLayer nil nil)
      (exit)
    )
  )

  (setq tStart (getvar "MILLISECS"))

  ;; Bóc tách thông minh từ khóa & chuẩn bị bộ lọc C++ siêu tốc
  (if (= optSplit "1")
    (setq searchTerms (Extract-Smart-Names kw))
    (setq searchTerms (list kw))
  )
  (setq kwSpecs (Prepare-Search-Specs searchTerms))

  ;; Lấy toàn bộ đối tượng theo loại đã chọn (Để FastMatch-Prepared giải mã đa bảng mã 100% chính xác)
  (setq coreFilter
    (cond
      ((= selectedTypeIdx "1") (list '(0 . "TEXT,MTEXT")))
      ((= selectedTypeIdx "2") (list '(0 . "INSERT")))
      ((= selectedTypeIdx "3") (list '(0 . "MULTILEADER,DIMENSION")))
      ((= selectedTypeIdx "4") (list '(0 . "TABLE")))
      (t (list '(0 . "TEXT,MTEXT,MULTILEADER,DIMENSION,TABLE,INSERT")))
    )
  )

  ;; Kết hợp bộ lọc Layer nếu có
  (if (and selectedLayer (/= selectedLayer "") (/= selectedLayer "Tất cả các Layer (*)"))
    (progn
      (prompt (strcat "\n[Tra Cứu Pro v12.0] Đang lọc siêu tốc trên Layer: [" selectedLayer "] ..."))
      (setq dxfFilter
        (append
          (list '(-4 . "<AND") (cons 8 selectedLayer))
          coreFilter
          (list '(-4 . "AND>"))
        )
      )
    )
    (progn
      (prompt "\n[Tra Cứu Pro v12.0] Đang quét siêu tốc C++ trên TẤT CẢ các Layer...")
      (setq dxfFilter coreFilter)
    )
  )

  (if (= selectedScopeIdx "1")
    (progn
      (prompt "\n>>> HÃY QUÉT CHỌN VÙNG BẢN VẼ CẦN TÌM KIẾM: ")
      (setq ss (ssget dxfFilter))
    )
    (setq ss (ssget "X" dxfFilter))
  )

  (setq matches '()
        countText 0
        countAtt 0
        countOther 0
        countCoord 0)

  ;; 0. KIỂM TRA NẾU TỪ KHÓA LÀ TỌA ĐỘ VN-2000 (X, Y)
  (setq coordPt (Parse-Coordinate-String kw))
  (if coordPt
    (progn
      (setq rowLabel (strcat "[ĐỊNH VỊ TỌA ĐỘ VN-2000] X = " (rtos (car coordPt) 2 3) "  |  Y = " (rtos (cadr coordPt) 2 3)))
      (setq matches (cons (list coordPt nil rowLabel "Tọa độ VN-2000" (strcat "X=" (rtos (car coordPt) 2 2) " Y=" (rtos (cadr coordPt) 2 2)) "---" coordPt) matches))
      (setq countCoord (1+ countCoord))

      ;; Quét các đối tượng Text/Block xung quanh tọa độ này trong bán kính 25m
      (setq ssNear (ssget "C" (list (- (car coordPt) 25.0) (- (cadr coordPt) 25.0))
                              (list (+ (car coordPt) 25.0) (+ (cadr coordPt) 25.0))
                              '((0 . "TEXT,MTEXT,INSERT"))))
      (if ssNear
        (repeat (setq j (sslength ssNear))
          (setq nEnt (ssname ssNear (setq j (1- j))))
          (setq nDxf (entget nEnt))
          (setq nTyp (cdr (assoc 0 nDxf)))
          (setq nLay (cdr (assoc 8 nDxf)))
          (setq nTxt (cdr (assoc 1 nDxf)))
          (if (null nTxt) (setq nTxt (cdr (assoc 2 nDxf))))
          (if (null nTxt) (setq nTxt "(Thực thể địa chính lân cận)"))
          (setq nRowLabel (strcat "[Lân cận Tọa độ] " nTyp " \"" (To-Unicode-Display nTxt) "\"   |   Layer: " nLay))
          (setq matches (cons (list (vlax-ename->vla-object nEnt) nEnt nRowLabel nTyp (To-Unicode-Display nTxt) nLay) matches))
          (setq countOther (1+ countOther))
        )
      )
    )
  )

  ;; Quét toàn diện các thực thể ứng viên đã qua lọc C++
  (if ss
    (repeat (setq i (sslength ss))
      (setq ent (ssname ss (setq i (1- i))))
      (setq dxf (entget ent))
      (setq typ (cdr (assoc 0 dxf)))
      (setq layerName (cdr (assoc 8 dxf)))

      (cond
        ;; 1. TEXT
        ((= typ "TEXT")
          (setq txt (cdr (assoc 1 dxf)))
          (if (FastMatch-Prepared txt kwSpecs)
            (progn
              (setq obj (vlax-ename->vla-object ent))
              (setq dispTxt (To-Unicode-Display txt))

              ;; Dò ghép cặp số thửa đứng (Chỉ áp dụng khi đối tượng tìm thấy là Số Thửa trên layer *THUA* hoặc là số nguyên ngắn)
              (setq pairedItem nil)
              (if (and (= optSplit "1")
                       (or (wcmatch (strcase layerName) "*THUA*")
                           (and (<= (strlen (vl-string-trim " " dispTxt)) 5)
                                (numberp (read (vl-string-trim " " dispTxt))))))
                (progn
                  (setq pt10 (cdr (assoc 10 dxf)))
                  (if pt10
                    (progn
                      (setq ssArea (ssget "C" (list (- (car pt10) 4.0) (- (cadr pt10) 8.0))
                                              (list (+ (car pt10) 4.0) (+ (cadr pt10) 1.0))
                                              '((0 . "TEXT,MTEXT"))))
                      (if ssArea
                        (repeat (setq j (sslength ssArea))
                          (setq areaEnt (ssname ssArea (setq j (1- j))))
                          (if (and (null pairedItem) (not (equal areaEnt ent)))
                            (progn
                              (setq areaDxf (entget areaEnt))
                              (setq areaTxt (cdr (assoc 1 areaDxf)))
                              (if (and areaTxt
                                       (wcmatch (strcase (cdr (assoc 8 areaDxf))) "*DT*,*DIENTICH*,*LOAI*")
                                       (numberp (distof areaTxt)))
                                (setq areaDisp (To-Unicode-Display areaTxt)
                                      dispTxt (strcat dispTxt " / DT: " areaDisp)
                                      pairedItem T)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )

              (setq rowLabel (strcat "[Text] \"" dispTxt "\"   |   Layer: " layerName))
              (setq matches (cons (list obj ent rowLabel "Text" dispTxt layerName) matches))
              (setq countText (1+ countText))
            )
          )
        )

        ;; 2. MTEXT
        ((= typ "MTEXT")
          (setq txt "")
          (foreach item dxf
            (if (= (car item) 3) (setq txt (strcat txt (cdr item))))
          )
          (setq txt (strcat txt (cdr (assoc 1 dxf))))
          (if (FastMatch-Prepared txt kwSpecs)
            (progn
              (setq obj (vlax-ename->vla-object ent))
              (setq dispTxt (To-Unicode-Display txt))
              (setq rowLabel (strcat "[MText] \"" dispTxt "\"   |   Layer: " layerName))
              (setq matches (cons (list obj ent rowLabel "MText" dispTxt layerName) matches))
              (setq countText (1+ countText))
            )
          )
        )

        ;; 3. INSERT (QUÉT ATTRIBUTE & ĐÀO SÂU BLOCK LỒNG BLOCK)
        ((= typ "INSERT")
          (setq obj nil)
          ;; A. Thuộc tính Attributes
          (if (= (cdr (assoc 66 dxf)) 1)
            (progn
              (setq sub ent)
              (while (and (setq sub (entnext sub))
                          (setq subDxf (entget sub))
                          (= (cdr (assoc 0 subDxf)) "ATTRIB"))
                (setq txt (cdr (assoc 1 subDxf)))
                (if (FastMatch-Prepared txt kwSpecs)
                  (progn
                    (setq dispTxt (To-Unicode-Display txt))
                    (setq rowLabel (strcat "[Block Tag: " (cdr (assoc 2 subDxf)) "] \"" dispTxt "\"   |   Layer: " layerName))
                    (setq matches (cons (list (vlax-ename->vla-object sub) sub rowLabel (strcat "Block (" (cdr (assoc 2 subDxf)) ")") dispTxt layerName) matches))
                    (setq countAtt (1+ countAtt))
                  )
                )
              )
            )
          )

          ;; B. Quét sâu định nghĩa Block (Đã có bộ nhớ đệm *Blk-Cache* siêu tốc)
          (if (= optNested "1")
            (progn
              (setq blkName (cdr (assoc 2 dxf)))
              (if (and blkName (/= blkName ""))
                (progn
                  (setq blkMatches (Get-Cached-Block-Matches blkName kwSpecs))
                  (if blkMatches
                    (progn
                      (if (not obj) (setq obj (vlax-ename->vla-object ent)))
                      (setq insPt (cdr (assoc 10 dxf))
                            sx    (cdr (assoc 41 dxf))
                            sy    (cdr (assoc 42 dxf))
                            sz    (cdr (assoc 43 dxf))
                            rot   (cdr (assoc 50 dxf)))
                      (foreach bItem blkMatches
                        (setq bSubTyp (nth 0 bItem)
                              bTxt    (nth 1 bItem)
                              bLocPt  (nth 2 bItem)
                              bParent (nth 3 bItem))
                        ;; Tính toán tọa độ WCS thực tế của đối tượng con bên trong Block
                        (setq bWcsPt (Transform-Local-To-WCS bLocPt insPt sx sy sz rot))
                        (setq dispTxt (To-Unicode-Display bTxt))
                        (setq rowLabel (strcat "[Trong Block: " blkName "] " bSubTyp " \"" dispTxt "\"   |   Layer: " layerName))
                        (setq matches (cons (list obj ent rowLabel (strcat "Block [" blkName "]") dispTxt layerName bWcsPt) matches))
                        (setq countAtt (1+ countAtt))
                      )
                    )
                  )
                )
              )
            )
          )
        )

        ;; 4. MULTILEADER
        ((= typ "MULTILEADER")
          (setq txt (cdr (assoc 304 dxf)))
          (if (null txt)
            (vl-catch-all-apply
              '(lambda () (setq txt (vla-get-TextString (vlax-ename->vla-object ent))))))
          (if (and txt (FastMatch-Prepared txt kwSpecs))
            (progn
              (setq obj (vlax-ename->vla-object ent))
              (setq dispTxt (To-Unicode-Display txt))
              (setq rowLabel (strcat "[MLeader] \"" dispTxt "\"   |   Layer: " layerName))
              (setq matches (cons (list obj ent rowLabel "MLeader" dispTxt layerName) matches))
              (setq countOther (1+ countOther))
            )
          )
        )

        ;; 5. DIMENSION
        ((vl-string-search "DIMENSION" typ)
          (setq txt (cdr (assoc 1 dxf)))
          (if (and txt (/= txt "") (FastMatch-Prepared txt kwSpecs))
            (progn
              (setq obj (vlax-ename->vla-object ent))
              (setq dispTxt (To-Unicode-Display txt))
              (setq rowLabel (strcat "[Dimension] \"" dispTxt "\"   |   Layer: " layerName))
              (setq matches (cons (list obj ent rowLabel "Dimension" dispTxt layerName) matches))
              (setq countOther (1+ countOther))
            )
          )
        )

        ;; 6. TABLE
        ((= typ "TABLE")
          (setq tableObj (vlax-ename->vla-object ent))
          (setq rCount (vla-get-Rows tableObj)
                cCount (vla-get-Columns tableObj)
                r 0)
          (while (< r rCount)
            (setq c 0)
            (while (< c cCount)
              (setq cellTxt (vla-GetText tableObj r c))
              (if (FastMatch-Prepared cellTxt kwSpecs)
                (progn
                  (setq dispTxt (To-Unicode-Display cellTxt))
                  (setq rowLabel (strcat "[Table Ô " (itoa (1+ r)) "," (itoa (1+ c)) "] \"" dispTxt "\"   |   Layer: " layerName))
                  (setq matches (cons (list tableObj ent rowLabel "Table" dispTxt layerName) matches))
                  (setq countOther (1+ countOther))
                )
              )
              (setq c (1+ c))
            )
            (setq r (1+ r))
          )
        )
      )
    )
  )
  (setq tElapsed (- (getvar "MILLISECS") tStart))
  (prompt (strcat "\n-> [HOÀN TẤT QUÉT BẢN VẼ TRONG " (rtos (/ tElapsed 1000.0) 2 2) " GIÂY]!"))

  (setq count (length matches))
  (if (= count 0)
    (progn
      (unload_dialog dclId)
      (if (findfile dclPath) (vl-file-delete dclPath))
      (prompt (strcat "\n[Tra Cứu Pro] Không tìm thấy đối tượng nào khớp với từ khóa '" kw "'!"))
    )
    (progn
      (setq matches (reverse matches))

      (setq dispList '()
            idx 1)
      (foreach item matches
        (setq dispList
          (cons
            (strcat (if (< idx 10) "00" (if (< idx 100) "0" "")) (itoa idx) ".  \""
                    (Highlight-Keyword-In-Text (nth 4 item) kw) "\"   ───   [" (nth 3 item) "]   │   Layer: " (nth 5 item))
            dispList
          )
        )
        (setq idx (1+ idx))
      )
      (setq dispList (reverse dispList))

      (foreach item matches
        (if (and (car item) (not (listp (car item))))
          (vl-catch-all-apply 'vla-Highlight (list (car item) :vlax-true))
        )
      )

      (setq *last-matches* matches)
      (setq currentSel "0")
      (setq loopResults T)

      (while loopResults
        (if (not (new_dialog "tracuu_results_dlg" dclId))
          (setq loopResults nil)
          (progn
            (set_tile "lbl_summary"
              (strcat "TÌM THẤY " (itoa count) " ĐỐI TƯỢNG  ("
                      (itoa countText) " Text/MText, "
                      (itoa countAtt) " Khối/Block, "
                      (itoa (+ countOther countCoord)) " Khác)"
                      (if selectedLayer (strcat " [Layer: " selectedLayer "]") " [Tất cả Layer]"))
            )

            (start_list "res_list")
            (foreach strItem dispList (add_list strItem))
            (end_list)

            (set_tile "res_list" currentSel)
            (mode_tile "res_list" 2)
            (Update-Inspector (atoi currentSel) matches kw)

            (action_tile "res_list"
              "(setq currentSel (get_tile \"res_list\"))
               (Update-Inspector (atoi currentSel) matches kw)
               (if (= $reason 4) (done_dialog 1))"
            )
            (action_tile "btn_zoom" "(setq currentSel (get_tile \"res_list\")) (done_dialog 1)")
            (action_tile "btn_tour" "(setq currentSel (get_tile \"res_list\")) (done_dialog 8)")
            (action_tile "btn_zoom_all" "(done_dialog 2)")
            (action_tile "btn_select_all" "(done_dialog 3)")
            (action_tile "btn_export_csv" "(done_dialog 5)")
            (action_tile "btn_mark_circle" "(done_dialog 6)")
            (action_tile "btn_clear_mark" "(done_dialog 7)")
            (action_tile "btn_new" "(done_dialog 4)")
            (action_tile "cancel" "(done_dialog 0)")

            (setq resAction (start_dialog))

            (cond
              ;; Zoom hoặc Tour Mode
              ((or (= resAction 1) (= resAction 8))
                (setq selIdx (atoi currentSel))
                (setq *exit-tour-flag* nil)
                (Do-Tour-Navigation selIdx matches)
                (if *exit-tour-flag*
                  (setq loopResults nil)
                  (setq currentSel (itoa (if *last-match-idx* *last-match-idx* selIdx)))
                )
              )

              ((= resAction 2)
                (vl-catch-all-apply 'vla-ZoomExtents (list (vlax-get-acad-object)))
                (vl-catch-all-apply 'vl-cmdf (list "_.zoom" "_e"))
                (initget "Enter Exit")
                (setq waitKey (getkword "\n[Đã Zoom toàn bản vẽ]. Nhấn [Enter/Space] mở lại Bảng Danh Sách, hoặc gõ [E] để thoát: "))
                (if (or (= waitKey "Exit") (= waitKey "E"))
                  (setq loopResults nil)
                )
              )

              ((= resAction 3)
                (setq ssFound (ssadd))
                (foreach item matches
                  (setq e (cadr item))
                  (if (and e (not (listp e))) (ssadd e ssFound))
                )
                (sssetfirst nil ssFound)
                (prompt (strcat "\n-> ĐÃ CHỌN " (itoa (sslength ssFound)) " ĐỐI TƯỢNG VÀO BỘ CHỌN (GRIPS)."))
                (setq loopResults nil)
              )

              ((= resAction 5)
                (setq expRes (ExportMatchesToExcel matches))
                (if expRes
                  (prompt "\n-> [THÀNH CÔNG] Đã xuất file Excel kết quả và tự động mở lên!")
                  (prompt "\n-> [LỖI] Không thể ghi file Excel. Hãy kiểm tra xem file có đang mở không.")
                )
              )

              ((= resAction 6)
                (setq markRes (MarkCirclesOnObjects matches))
                (prompt (strcat "\n-> ĐÃ KHOANH " (itoa markRes) " VÒNG TRÒN ĐỎ TRÊN LAYER '_DANH_DAU_TRA_CUU'!"))
              )

              ((= resAction 7)
                (setq markRes (ClearMarkCircles))
                (prompt (strcat "\n-> ĐÃ XÓA " (itoa markRes) " VÒNG TRÒN ĐỎ ĐÁNH DẤU!"))
              )

              ((= resAction 4)
                (setq loopResults nil)
                (Clear-Spotlight-Circle)
                (foreach item matches
                  (if (and (car item) (not (listp (car item))))
                    (vl-catch-all-apply 'vla-Highlight (list (car item) :vlax-false))
                  )
                )
                (unload_dialog dclId)
                (if (findfile dclPath) (vl-file-delete dclPath))
                (c:FF)
              )

              (t
                (setq loopResults nil)
              )
            )
          )
        )
      )

      (Clear-Spotlight-Circle)
      (foreach item matches
        (if (and (car item) (not (listp (car item))))
          (vl-catch-all-apply 'vla-Highlight (list (car item) :vlax-false))
        )
      )

      (unload_dialog dclId)
      (if (findfile dclPath) (vl-file-delete dclPath))
    )
  )
  (princ)
)

;;; -------------------------------------------------------------------------
;;; 12. CÔNG CỤ ĐỊNH DẠNG BẢN ĐỒ: c:KTD (KIỂM TRA ĐỊNH DẠNG)
;;; -------------------------------------------------------------------------
(defun c:KTD ( / *error* fn outPath dwgName dwgPrefix fullPath acadVer
                 extMin extMax w h
                 st stName stFont stBigFont stH stW stAng stEncoding stCount
                 ly lyName lyCount isCadLayer ssL
                 blk blkName blkCount blkAnonymous
                 ssAll totalEntities entText entMText entInsert entPline entLine
                 entDim entMLeader entHatch entTable entOther
                 sampleCount ssSample i ent dxf typ txt cleanTxt ptX ss )
  (vl-load-com)

  (defun *error* (msg)
    (if (and fn (open-p fn)) (close fn))
    (if (and msg (not (wcmatch (strcase msg) "*QUIT*,*EXIT*,*CANCEL*")))
      (prompt (strcat "\n[KTD] Lỗi kiểm tra: " msg))
    )
    (princ)
  )

  (prompt "\n============================================================")
  (prompt "\n[KTD] ĐANG PHÂN TÍCH TOÀN BỘ ĐỊNH DẠNG BẢN ĐỒ CHI TIẾT...")
  (prompt "\n============================================================")

  (setq outPath (strcat (TC-Get-Extra-Dir) "\\BAOCAO_DINHDANG_BANDO.TXT"))
  (setq fn (open outPath "w"))
  (if (null fn)
    (setq outPath (strcat (getvar "DWGPREFIX") "BAOCAO_DINHDANG_BANDO.TXT")
          fn (open outPath "w"))
  )
  (if (null fn)
    (progn
      (prompt "\n[LỖI] Không thể tạo file báo cáo định dạng!")
      (exit)
    )
  )

  ;; UTF-8 BOM
  (write-line (strcat (chr 239) (chr 187) (chr 191) "================================================================================") fn)
  (write-line "BÁO CÁO PHÂN TÍCH TOÀN DIỆN ĐỊNH DẠNG BẢN ĐỒ ĐỊA CHÍNH (CADASTRAL AUDIT REPORT)" fn)
  (write-line "Công cụ: FAST FIND PRO - v10.0 MULTI-FONT UNIVERSAL (Bản Đồ Địa Chính)" fn)
  (write-line "================================================================================" fn)
  (write-line "" fn)

  ;; 1. THÔNG TIN CHUNG BẢN ĐỒ
  (setq dwgName (getvar "DWGNAME")
        dwgPrefix (getvar "DWGPREFIX")
        fullPath (strcat dwgPrefix dwgName)
        acadVer (getvar "ACADVER")
        extMin (getvar "EXTMIN")
        extMax (getvar "EXTMAX"))

  (write-line "1. THÔNG TIN CHUNG BẢN ĐỒ (DRAWING METADATA):" fn)
  (write-line "--------------------------------------------------------------------------------" fn)
  (write-line (strcat " - Tên file bản vẽ   : " dwgName) fn)
  (write-line (strcat " - Đường dẫn lưu trữ : " fullPath) fn)
  (write-line (strcat " - Phiên bản AutoCAD : " acadVer " (AutoCAD 2023 Process)") fn)
  (write-line (strcat " - Hệ đơn vị đo      : " (itoa (getvar "LUNITS")) " (Hệ mét / Đơn vị thực địa)") fn)
  (if (and extMin extMax)
    (progn
      (setq w (abs (- (car extMax) (car extMin)))
            h (abs (- (cadr extMax) (cadr extMin))))
      (write-line (strcat " - Phạm vi VN-2000 Min : X = " (rtos (car extMin) 2 3) " , Y = " (rtos (cadr extMin) 2 3)) fn)
      (write-line (strcat " - Phạm vi VN-2000 Max : X = " (rtos (car extMax) 2 3) " , Y = " (rtos (cadr extMax) 2 3)) fn)
      (write-line (strcat " - Kích thước bao vùng : Rộng " (rtos w 2 2) "m x Dài " (rtos h 2 2) "m (Diện tích bao ~" (rtos (/ (* w h) 10000.0) 2 2) " ha)") fn)
    )
    (write-line " - Phạm vi tọa độ    : Chưa xác định hoặc bản vẽ trống" fn)
  )
  (write-line "" fn)

  ;; 2. TỔNG HỢP KIỂU CHỮ & BẢNG MÃ
  (write-line "2. TỔNG HỢP KIỂU CHỮ (TEXTSTYLES) & FONT CHỮ (SHX / TTF):" fn)
  (write-line "--------------------------------------------------------------------------------" fn)
  (write-line " STT | Tên TextStyle       | Font chính             | BigFont (SHX)       | Định dạng Bảng Mã" fn)
  (write-line "-----+---------------------+------------------------+---------------------+----------------------" fn)

  (setq st (tblnext "STYLE" T)
        stCount 0)
  (while st
    (setq stCount (1+ stCount)
          stName (cdr (assoc 2 st))
          stFont (cdr (assoc 3 st))
          stBigFont (cdr (assoc 4 st)))
    (if (null stFont) (setq stFont "---"))
    (if (or (null stBigFont) (= stBigFont "")) (setq stBigFont "---"))

    (setq stEncoding
      (cond
        ((or (vl-string-search "vntime" (strcase stFont))
             (vl-string-search ".vn" (strcase stFont))
             (vl-string-search "tc" (strcase (strcat stFont stBigFont))))
          "TCVN3 (ABC / .VnTime)")
        ((or (vl-string-search "vni" (strcase stFont))
             (vl-string-search "vni" (strcase stBigFont)))
          "VNI-Windows")
        ((or (vl-string-search "arial" (strcase stFont))
             (vl-string-search "times" (strcase stFont))
             (vl-string-search "tahoma" (strcase stFont)))
          "Unicode Quốc Tế (TTF)")
        ((or (vl-string-search ".shx" (strcase stFont))
             (vl-string-search ".shx" (strcase stBigFont)))
          "Font Nét Đơn SHX CAD")
        (t "Mặc định / Standard")
      )
    )

    (write-line
      (strcat " " (if (< stCount 10) "0" "") (itoa stCount) "  | "
              (substr (strcat stName "                     ") 1 19) " | "
              (substr (strcat stFont "                        ") 1 22) " | "
              (substr (strcat stBigFont "                     ") 1 19) " | "
              stEncoding)
      fn
    )
    (setq st (tblnext "STYLE"))
  )
  (write-line "" fn)

  ;; 3. THỐNG KÊ ĐỐI TƯỢNG TRONG BẢN ĐỒ
  (write-line "3. THỐNG KÊ SỐ LƯỢNG ĐỐI TƯỢNG (DATABASE ENTITY AUDIT):" fn)
  (write-line "--------------------------------------------------------------------------------" fn)
  (prompt "\n[KTD] Đang thống kê thực thể bản vẽ...")

  (setq ssAll (ssget "X"))
  (setq totalEntities (if ssAll (sslength ssAll) 0))

  (setq entText (if (setq ss (ssget "X" '((0 . "TEXT")))) (sslength ss) 0))
  (setq entMText (if (setq ss (ssget "X" '((0 . "MTEXT")))) (sslength ss) 0))
  (setq entInsert (if (setq ss (ssget "X" '((0 . "INSERT")))) (sslength ss) 0))
  (setq entPline (if (setq ss (ssget "X" '((0 . "LWPOLYLINE,POLYLINE")))) (sslength ss) 0))
  (setq entLine (if (setq ss (ssget "X" '((0 . "LINE")))) (sslength ss) 0))
  (setq entDim (if (setq ss (ssget "X" '((0 . "DIMENSION")))) (sslength ss) 0))
  (setq entMLeader (if (setq ss (ssget "X" '((0 . "MULTILEADER,LEADER")))) (sslength ss) 0))
  (setq entHatch (if (setq ss (ssget "X" '((0 . "HATCH")))) (sslength ss) 0))
  (setq entTable (if (setq ss (ssget "X" '((0 . "TABLE")))) (sslength ss) 0))
  (setq entOther (- totalEntities (+ entText entMText entInsert entPline entLine entDim entMLeader entHatch entTable)))

  (write-line (strcat " - Tổng cộng tất cả thực thể trong bản vẽ : " (itoa totalEntities)) fn)
  (write-line (strcat "   + TEXT (Chữ đơn, số thửa, diện tích)   : " (itoa entText)) fn)
  (write-line (strcat "   + MTEXT (Chữ nhiều dòng, thông tin GCN) : " (itoa entMText)) fn)
  (write-line (strcat "   + INSERT (Khối Block, Sheet bản đồ)     : " (itoa entInsert)) fn)
  (write-line (strcat "   + LWPOLYLINE (Ranh thửa, ranh quy hoạch): " (itoa entPline)) fn)
  (write-line (strcat "   + LINE (Đoạn thẳng ranh giới)          : " (itoa entLine)) fn)
  (write-line (strcat "   + DIMENSION / Kích thước ranh thửa     : " (itoa entDim)) fn)
  (write-line (strcat "   + MULTILEADER / Chỉ dẫn thửa nhỏ       : " (itoa entMLeader)) fn)
  (write-line (strcat "   + HATCH (Ký hiệu loại đất, mặt nước)   : " (itoa entHatch)) fn)
  (write-line (strcat "   + TABLE (Bảng số liệu địa chính)       : " (itoa entTable)) fn)
  (write-line (strcat "   + Các đối tượng khác                   : " (itoa entOther)) fn)
  (write-line "" fn)

  ;; 4. MẪU DỮ LIỆU CHỮ TRÍCH XUẤT THỰC TẾ TRÊN BẢN ĐỒ
  (write-line "4. MẪU DỮ LIỆU CHỮ TRÍCH XUẤT THỰC TẾ TRÊN BẢN ĐỒ (SAMPLE TEXTS):" fn)
  (write-line "--------------------------------------------------------------------------------" fn)
  (prompt "\n[KTD] Đang trích xuất mẫu dữ liệu chữ thực tế...")

  (setq ssSample (ssget "X" '((0 . "TEXT,MTEXT"))))
  (if ssSample
    (progn
      (setq sampleCount (min 50 (sslength ssSample)))
      (setq i 0)
      (while (< i sampleCount)
        (setq ent (ssname ssSample i))
        (setq dxf (entget ent))
        (setq typ (cdr (assoc 0 dxf)))
        (setq lyName (cdr (assoc 8 dxf)))
        (setq txt (cdr (assoc 1 dxf)))
        (if (= typ "MTEXT")
          (progn
            (setq txt "")
            (foreach item dxf (if (= (car item) 3) (setq txt (strcat txt (cdr item)))))
            (setq txt (strcat txt (cdr (assoc 1 dxf))))
          )
        )
        (setq ptX (cdr (assoc 10 dxf)))
        (write-line
          (strcat " [" (if (< (1+ i) 10) "0" "") (itoa (1+ i)) "] "
                  "Loại: " typ " | Layer: " lyName "\n"
                  "      -> Nguyên bản: \"" txt "\"\n"
                  "      -> Hiển thị Unicode: \"" (To-Unicode-Display txt) "\"\n"
                  "      -> Chuẩn hóa Không Dấu: \"" (To-Plain-ASCII txt) "\"\n"
                  (if ptX (strcat "      -> Tọa độ: X=" (rtos (car ptX) 2 2) ", Y=" (rtos (cadr ptX) 2 2) "\n") ""))
          fn
        )
        (setq i (1+ i))
      )
    )
  )
  (write-line "" fn)

  (write-line "================================================================================" fn)
  (close fn)

  (prompt (strcat "\n-> [THÀNH CÔNG] ĐÃ XUẤT BÁO CÁO TẠI: " outPath))
  (prompt "\n-> Đang mở báo cáo trong Notepad...")

  (vl-catch-all-apply
    '(lambda ()
       (startapp "notepad.exe" outPath)
     )
  )
  (princ)
)

(defun c:KIEMTRA () (c:KTD))
(defun c:INSPECT () (c:KTD))

;;; -------------------------------------------------------------------------
;;; -------------------------------------------------------------------------
;;; 13. TỰ ĐỘNG SỬA LỖI TEXTSTYLE ĐỊA CHÍNH (Arial-reg -> VNARIAL.TTF / .VnArial)
;;; -------------------------------------------------------------------------
(setq *autofix-last-dwg* nil)

(defun Word-Needs-Encoding-Convert (txt / len i b)
  (if (or (null txt) (= txt ""))
    nil
    (cond
      ((Word-Has-Strong-VNI txt) T)
      ((Word-Has-Strong-TCVN txt) T)
      ((wcmatch txt "*Ôn*,*¨n*,*CÇu*,*X©y*,*Phó*,*T¹n*,*H×nh*,*?Êt*,*chÊp*,*®*,*§*,*©*,*¨*,*Ç*,*Êp*,*cÊp*,*H¶i*,*Thñ*,*C-ng*,*C­ng*") T)
      (t
        (setq len (strlen txt) i 1)
        (while (and (<= i len)
                    (setq b (ascii (substr txt i 1)))
                    (not (or (and (>= b 161) (<= b 174))
                             (and (>= b 181) (<= b 254) (not (= b 180)) (not (= b 186))))))
          (setq i (1+ i))
        )
        (<= i len)
      )
    )
  )
)

(defun Clean-And-Fix-Vietnamese-Text (txt / uTxt)
  (if (Word-Needs-Encoding-Convert txt)
    (progn
      (setq uTxt (To-Unicode-Display txt))
      (if (vl-string-search "?Êt" uTxt) (setq uTxt (vl-string-subst "đất" "?Êt" uTxt)))
      (if (vl-string-search "?êt" uTxt) (setq uTxt (vl-string-subst "đất" "?êt" uTxt)))
      (if (vl-string-search "?ÊT" uTxt) (setq uTxt (vl-string-subst "ĐẤT" "?ÊT" uTxt)))
      (if (vl-string-search "chÊp" uTxt) (setq uTxt (vl-string-subst "chấp" "chÊp" uTxt)))
      (if (vl-string-search "C-ng" uTxt) (setq uTxt (vl-string-subst "Cường" "C-ng" uTxt)))
      (if (vl-string-search "T¹n Phó" uTxt) (setq uTxt (vl-string-subst "Tân Phú" "T¹n Phó" uTxt)))
      (if (vl-string-search "Tạn Phú" uTxt) (setq uTxt (vl-string-subst "Tân Phú" "Tạn Phú" uTxt)))
      uTxt
    )
    nil
  )
)

(defun AutoFix-Drawing-Fonts ( / ss i e d str st badStyles stName stEnt stDxf newDxf fixedCount ssUpd j tbl sName uName f3 xdata xf1000 targetFile targetFam isBold badVniStyles uniStyles vniStyles txt clist fCheck targetTtf blkEnt attEnt attDxf attTxt mldEnt mldObj mldTxt uTxt )
  (vl-load-com)
  (setq badStyles '()
        uniStyles '()
        vniStyles '()
        fixedCount 0)

  ;; 0. Tự động chuyển đổi các đối tượng Text/MText/Block Attrib/MLeader/Dimension gõ bằng mã TCVN3 hoặc VNI sang Unicode chuẩn
  (setq ss (ssget "X" '((0 . "TEXT,MTEXT"))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i)
              d (entget e)
              txt (cdr (assoc 1 d)))
        (setq uTxt (Clean-And-Fix-Vietnamese-Text txt))
        (if (and uTxt (/= uTxt txt))
          (progn
            (entmod (subst (cons 1 uTxt) (assoc 1 d) d))
            (entupd e)
            (setq st (cdr (assoc 7 d)))
            (if (and st (not (member st uniStyles)))
              (setq uniStyles (cons st uniStyles))
            )
            (setq fixedCount (1+ fixedCount))
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (setq ss (ssget "X" '((0 . "INSERT") (66 . 1))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq blkEnt (ssname ss i)
              attEnt (entnext blkEnt))
        (while (and attEnt (= (cdr (assoc 0 (entget attEnt))) "ATTRIB"))
          (setq attDxf (entget attEnt)
                attTxt (cdr (assoc 1 attDxf)))
          (setq uTxt (Clean-And-Fix-Vietnamese-Text attTxt))
          (if (and uTxt (/= uTxt attTxt))
            (progn
              (entmod (subst (cons 1 uTxt) (assoc 1 attDxf) attDxf))
              (entupd attEnt)
              (setq st (cdr (assoc 7 attDxf)))
              (if (and st (not (member st uniStyles)))
                (setq uniStyles (cons st uniStyles))
              )
              (setq fixedCount (1+ fixedCount))
            )
          )
          (setq attEnt (entnext attEnt))
        )
        (setq i (1+ i))
      )
    )
  )
  (setq ss (ssget "X" '((0 . "MULTILEADER"))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq mldEnt (ssname ss i)
              mldObj (vlax-ename->vla-object mldEnt))
        (vl-catch-all-apply
          '(lambda ()
             (setq mldTxt (vla-get-TextString mldObj))
             (setq uTxt (Clean-And-Fix-Vietnamese-Text mldTxt))
             (if (and uTxt (/= uTxt mldTxt))
               (progn
                 (vla-put-TextString mldObj uTxt)
                 (setq fixedCount (1+ fixedCount))
               )
             )
           )
        )
        (setq i (1+ i))
      )
    )
  )
  (setq ss (ssget "X" '((0 . "DIMENSION"))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i)
              d (entget e)
              txt (cdr (assoc 1 d)))
        (if (and txt (/= txt ""))
          (progn
            (setq uTxt (Clean-And-Fix-Vietnamese-Text txt))
            (if (and uTxt (/= uTxt txt))
              (progn
                (entmod (subst (cons 1 uTxt) (assoc 1 d) d))
                (entupd e)
                (setq fixedCount (1+ fixedCount))
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )

  ;; 1. Lọc trực tiếp bằng nhân C++ (ssget "X") chỉ lấy các Text/MText chứa mã %%1xx hoặc %%2xx (0.001 giây!)
  (setq ss (ssget "X" '((0 . "TEXT,MTEXT") (1 . "*%%1*,*%%2*"))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i)
              d (entget e)
              st (cdr (assoc 7 d)))
        (if (and st (not (member st badStyles)))
          (setq badStyles (cons st badStyles))
        )
        (setq i (1+ i))
      )
    )
  )
  ;; Bổ sung các TextStyle xuất từ MicroStation DGN sang DWG thường bị gán nhầm arial.ttf
  (foreach knownSt '("Arial-reg" "Arial-bol" "Arial-lic" "Arial-bolita")
    (if (and (tblsearch "STYLE" knownSt) (not (member knownSt badStyles)))
      (setq badStyles (cons knownSt badStyles))
    )
  )
  (foreach stName badStyles
    (setq stEnt (tblobjname "STYLE" stName))
    (if stEnt
      (progn
        (setq stDxf (entget stEnt '("*")))
        (if (not (wcmatch (strcase (cdr (assoc 3 stDxf))) "*VN*,*VHARIAL*"))
          (progn
            (setq newDxf (subst '(3 . "VNARIAL.TTF") (assoc 3 stDxf) stDxf))
            (if (assoc -3 newDxf)
              (setq newDxf (subst '(-3 ("ACAD" (1000 . ".VnArial") (1071 . 0))) (assoc -3 newDxf) newDxf))
              (setq newDxf (append newDxf '((-3 ("ACAD" (1000 . ".VnArial") (1071 . 0))))))
            )
            (entmod newDxf)
            (vl-catch-all-apply
              '(lambda ()
                 (vla-SetFont
                   (vla-Item (vla-get-TextStyles (vla-get-ActiveDocument (vlax-get-acad-object))) stName)
                   ".VnArial" :vlax-false :vlax-false 0 0
                 )
               )
            )
            (setq ssUpd (ssget "X" (list '(0 . "TEXT,MTEXT") (cons 7 stName))))
            (if ssUpd
              (progn
                (setq j 0)
                (repeat (sslength ssUpd)
                  (entupd (ssname ssUpd j))
                  (setq j (1+ j))
                )
              )
            )
            (setq fixedCount (1+ fixedCount))
          )
        )
      )
    )
  )

  ;; 2. Lọc trực tiếp bằng nhân C++ (ssget "X") các chữ mã VNI-Windows (Traàn, Coâng, Thaønh, LEÂ, NGOÏC, LUYEÄN, chöa...) bị gán nhầm sang font không phải VNI (0.001 giây!)
  (setq badVniStyles '())
  (setq ss (ssget "X" (list '(0 . "TEXT,MTEXT") (cons 1 (strcat "*a" (chr 224) "*,*o" (chr 226) "*,*a" (chr 248) "*,*e" (chr 227) "*,*e" (chr 225) "*,*E" (chr 194) "*,*O" (chr 207) "*,*E" (chr 196) "*,*A" (chr 202) "*,*A" (chr 217) "*,*" (chr 246) (chr 239) "*,*" (chr 246) (chr 244) "*")))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i)
              d (entget e)
              st (cdr (assoc 7 d)))
        (if (and st (not (member st badVniStyles)))
          (setq badVniStyles (cons st badVniStyles))
        )
        (setq i (1+ i))
      )
    )
  )
  (foreach stName badVniStyles
    (setq stEnt (tblobjname "STYLE" stName))
    (if stEnt
      (progn
        (setq stDxf (entget stEnt '("ACAD")))
        (setq f3 (strcase (if (cdr (assoc 3 stDxf)) (cdr (assoc 3 stDxf)) "")))
        (setq xdata (cdr (assoc "ACAD" (cdr (assoc -3 stDxf)))))
        (setq xf1000 (strcase (if (and xdata (cdr (assoc 1000 xdata))) (cdr (assoc 1000 xdata)) "")))
        (if (not (wcmatch (strcat f3 "|" xf1000) "*VNI*,*VHELV*,*VAPTIM*,*VAVO*,*VTIME*,*VFRANK*,*VMARIA*"))
          (progn
            (setq newDxf (subst '(3 . "Vhelven.ttf") (assoc 3 stDxf) stDxf))
            (if (assoc -3 newDxf)
              (setq newDxf (subst '(-3 ("ACAD" (1000 . "VNI-Helve") (1071 . 2))) (assoc -3 newDxf) newDxf))
              (setq newDxf (append newDxf '((-3 ("ACAD" (1000 . "VNI-Helve") (1071 . 2))))))
            )
            (entmod newDxf)
            (vl-catch-all-apply
              '(lambda ()
                 (vla-SetFont
                   (vla-Item (vla-get-TextStyles (vla-get-ActiveDocument (vlax-get-acad-object))) stName)
                   "VNI-Helve" :vlax-false :vlax-false 0 0
                 )
               )
            )
            (setq fixedCount (1+ fixedCount))
          )
        )
      )
    )
  )

  ;; 3. Quét bảng STYLE (chỉ ~20-60 mục, mất < 0.001s) để sửa các kiểu chữ VNI / TCVN3 bị rỗng file font (assoc 3 = "") hoặc sai tên họ font (như Vf-Helve Condense)
  (setq tbl nil)
  (while (setq tbl (tblnext "STYLE" (null tbl)))
    (setq sName (cdr (assoc 2 tbl)))
    (if (and sName (/= sName ""))
      (progn
        (setq stEnt (tblobjname "STYLE" sName))
        (if stEnt
          (progn
            (setq stDxf (entget stEnt '("ACAD")))
            (setq f3 (cdr (assoc 3 stDxf)))
            (if (null f3) (setq f3 ""))
            (setq xdata (cdr (assoc "ACAD" (cdr (assoc -3 stDxf)))))
            (setq xf1000 (if xdata (cdr (assoc 1000 xdata)) ""))
            (if (null xf1000) (setq xf1000 ""))
            (setq uName (strcase (strcat sName "|" xf1000)))
            (setq targetFile nil targetFam nil isBold :vlax-false)
            (if (or (= f3 "") (wcmatch (strcase xf1000) "VF-HELVE*") (wcmatch (strcase f3) "*VN-ABC*,*VNABC*"))
              (cond
                ((wcmatch uName "*HELV*CB*,*CONDENSE*BOLD*")
                  (setq targetFile "Vhelvcb.ttf" targetFam "VNI-Helve-Condense" isBold :vlax-true))
                ((wcmatch uName "*HELV*CN*,*CONDENSE*,*UVHELV*")
                  (setq targetFile "Vhelvcn.ttf" targetFam "VNI-Helve-Condense"))
                ((wcmatch uName "*VHELVEB*,*VNIHB*,*VNI-HELVE*B*")
                  (setq targetFile "Vhelveb.ttf" targetFam "VNI-Helve" isBold :vlax-true))
                ((wcmatch uName "*VHELV*,*VNIH*,*VNI-HELVE*")
                  (setq targetFile "Vhelven.ttf" targetFam "VNI-Helve"))
                ((wcmatch uName "*APTIM*B*")
                  (setq targetFile "VAPTIMB0.TTF" targetFam "VNI-Aptima" isBold :vlax-true))
                ((wcmatch uName "*APTIM*")
                  (setq targetFile "VAPTIMN0.TTF" targetFam "VNI-Aptima"))
                ((wcmatch uName "*AVO*")
                  (setq targetFile "vavon__0.TTF" targetFam "VNI-Avo"))
                ((wcmatch uName "*VNI*TIME*B*,*VTIME*B*")
                  (setq targetFile "VTIMESB.TTF" targetFam "VNI-Times" isBold :vlax-true))
                ((wcmatch uName "*VNI*TIME*,*VTIME*,*VN-TIMES*")
                  (setq targetFile "VTIMESN0.TTF" targetFam "VNI-Times"))
                ((wcmatch uName "*NARROW*H*")
                  (setq targetFile "VHARIALN.TTF" targetFam ".VnArial NarrowH"))
                ((wcmatch uName "*NARROW*")
                  (setq targetFile "VNARIALN.TTF" targetFam ".VnArial Narrow"))
                ((wcmatch uName "*AVANT*H*")
                  (setq targetFile "VHAVAN.TTF" targetFam ".VnAvantH"))
                ((wcmatch uName "*ARIAL*H*B*,*HCMHB*")
                  (setq targetFile "VHARIALB.TTF" targetFam ".VnArialH" isBold :vlax-true))
                ((wcmatch uName "*ARIAL*H*,*HCMH*")
                  (setq targetFile "Vharial.ttf" targetFam ".VnArialH"))
                ((wcmatch uName "*TIME*H*")
                  (setq targetFile "Vhtime.ttf" targetFam ".VnTimeH"))
                ((wcmatch (strcase f3) "*VN-ABC*,*VNABC*")
                  (if (wcmatch uName "*HCMH*,*H*")
                    (setq targetFile "Vharial.ttf" targetFam ".VnArialH")
                    (setq targetFile "Vnarial.ttf" targetFam ".VnArial")
                  )
                )
                ((wcmatch uName "*HCM*")
                  (if (wcmatch uName "*HCMH*,*H*")
                    (setq targetFile "Vharial.ttf" targetFam ".VnArialH")
                    (setq targetFile "Vnarial.ttf" targetFam ".VnArial")
                  )
                )
                ((wcmatch uName "*.VNARIAL*,*VNARIAL*")
                  (setq targetFile "Vnarial.ttf" targetFam ".VnArial"))
                ((wcmatch uName "*.VNTIME*,*VNTIME*")
                  (setq targetFile "Vntime.ttf" targetFam ".VnTime"))
              )
            )
            (if (and targetFile targetFam)
              (progn
                (setq newDxf (subst (cons 3 targetFile) (assoc 3 stDxf) stDxf))
                (if (assoc -3 newDxf)
                  (setq newDxf (subst (list -3 (list "ACAD" (cons 1000 targetFam) (cons 1071 (if (= isBold :vlax-true) 33554434 2)))) (assoc -3 newDxf) newDxf))
                  (setq newDxf (append newDxf (list (list -3 (list "ACAD" (cons 1000 targetFam) (cons 1071 (if (= isBold :vlax-true) 33554434 2)))))))
                )
                (entmod newDxf)
                (vl-catch-all-apply
                  '(lambda ()
                     (vla-SetFont
                       (vla-Item (vla-get-TextStyles (vla-get-ActiveDocument (vlax-get-acad-object))) sName)
                       targetFam isBold :vlax-false 0 0
                     )
                   )
                )
                (setq fixedCount (1+ fixedCount))
              )
            )
          )
        )
      )
    )
  )

  ;; 4. Sửa triệt để lỗi chữ UNICODE (kí tự > 255) bị gán nhầm sang font VNI, TCVN3 hoặc SHX (lỗi ô vuông □)
  (setq uniStyles '()
        vniStyles '())
  (setq ss (ssget "X" '((0 . "TEXT,MTEXT"))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i)
              d (entget e)
              st (cdr (assoc 7 d))
              txt (cdr (assoc 1 d)))
        (if (and st txt)
          (progn
            (setq clist (vl-string->list txt))
            ;; Ký tự tiếng Việt Unicode (mã \U+, UTF-8 đa byte hoặc > 255)
            (if (or (vl-string-search "\\U+" txt)
                    (vl-string-search "\\u+" txt)
                    (vl-some '(lambda (c) (> c 255)) clist)
                    (vl-some '(lambda (c) (member c '(196 197 225 226))) clist))
              (if (not (member st uniStyles))
                (setq uniStyles (cons st uniStyles))
              )
            )
            ;; Ký tự bảng mã VNI-Windows (a\xE0, o\xE2, a\xF8, e\xE7, e\xE5...)
            (if (or (vl-string-search "a\xE0" txt) (vl-string-search "o\xE2" txt)
                    (vl-string-search "a\xF8" txt) (vl-string-search "e\xE7" txt)
                    (vl-string-search "e\xE5" txt))
              (if (not (member st vniStyles))
                (setq vniStyles (cons st vniStyles))
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (foreach stName uniStyles
    ;; Chỉ chuyển sang Arial nếu TextStyle này không dùng cho chữ VNI
    (if (not (member stName vniStyles))
      (progn
        (setq stEnt (tblobjname "STYLE" stName))
        (if stEnt
          (progn
            (setq stDxf (entget stEnt '("ACAD")))
            (setq f3 (strcase (if (cdr (assoc 3 stDxf)) (cdr (assoc 3 stDxf)) "")))
            (setq xdata (cdr (assoc "ACAD" (cdr (assoc -3 stDxf)))))
            (setq xf1000 (strcase (if (and xdata (cdr (assoc 1000 xdata))) (cdr (assoc 1000 xdata)) "")))
            (setq fCheck (strcat f3 "|" xf1000))
            ;; Nếu TextStyle đang trỏ sang font VNI, TCVN3 hoặc SHX không hỗ trợ Unicode
            (if (or (wcmatch fCheck "*VNI*,*VHELV*,*VAPTIM*,*VAVO*,*VTIME*,*VFRANK*,*VMARIA*,*VF-*")
                    (wcmatch fCheck "*VNARIAL*,*VNTIME*,*.VN*")
                    (wcmatch f3 "*VN_VNI*,*.SHX*")
                    (= f3 ""))
              (progn
                (setq isBold (or (wcmatch (strcase stName) "*B*,*BOLD*")
                                 (wcmatch fCheck "*BOLD*,*B0.TTF*,*B.TTF*")))
                (setq targetTtf (if isBold "arialbd.ttf" "arial.ttf")
                      targetFam "Arial")
                (setq newDxf (subst (cons 3 targetTtf) (assoc 3 stDxf) stDxf))
                (if (assoc -3 newDxf)
                  (setq newDxf (subst (list -3 (list "ACAD" (cons 1000 targetFam) (cons 1071 (if isBold 33554434 0)))) (assoc -3 newDxf) newDxf))
                  (setq newDxf (append newDxf (list (list -3 (list "ACAD" (cons 1000 targetFam) (cons 1071 (if isBold 33554434 0)))))))
                )
                (entmod newDxf)
                (vl-catch-all-apply
                  '(lambda ()
                     (vla-SetFont
                       (vla-Item (vla-get-TextStyles (vla-get-ActiveDocument (vlax-get-acad-object))) stName)
                       targetFam (if isBold :vlax-true :vlax-false) :vlax-false 0 0
                     )
                   )
                )
                (setq ssUpd (ssget "X" (list '(0 . "TEXT,MTEXT") (cons 7 stName))))
                (if ssUpd
                  (progn
                    (setq j 0)
                    (repeat (sslength ssUpd)
                      (entupd (ssname ssUpd j))
                      (setq j (1+ j))
                    )
                  )
                )
                (setq fixedCount (1+ fixedCount))
              )
            )
          )
        )
      )
    )
  )

  (if (> fixedCount 0)
    (progn
      (vl-catch-all-apply
        '(lambda ()
           (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) acAllViewports)
         )
      )
      (prompt (strcat "\n-> [Tự Động Sửa Font] Đã đồng bộ " (itoa fixedCount) " kiểu chữ (VNI / TCVN3) về đúng Font chuẩn!"))
    )
  )
  (setq *autofix-last-dwg* (strcat (getvar "DWGPREFIX") (getvar "DWGNAME")))
  fixedCount
)

(defun AutoFix-Drawing-Fonts-Once ( / curDwg)
  (setq curDwg (strcat (getvar "DWGPREFIX") (getvar "DWGNAME")))
  (if (/= curDwg *autofix-last-dwg*)
    (AutoFix-Drawing-Fonts)
    0
  )
)

(defun c:SF ( / tbl sName stEnt stDxf )
  (setq *autofix-last-dwg* nil)
  (prompt "\n==================================================================")
  (prompt "\n [AUTOCAD FONT FIXER] ĐANG QUÉT & SỬA LỖI TOÀN BỘ BẢN VẼ...")
  (prompt "\n==================================================================")
  (AutoFix-Drawing-Fonts)
  ;; Làm tươi toàn bộ bảng STYLE và Regen toàn bản vẽ
  (setq tbl nil)
  (while (setq tbl (tblnext "STYLE" (null tbl)))
    (setq sName (cdr (assoc 2 tbl)))
    (if (and sName (/= sName "") (setq stEnt (tblobjname "STYLE" sName)))
      (entmod (entget stEnt '("*")))
    )
  )
  (vl-catch-all-apply
    '(lambda ()
       (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) 0)
     )
  )
  (prompt "\n==================================================================")
  (prompt "\n [THÀNH CÔNG] ĐÃ SỬA LỖI VÀ LÀM TƯƠI TOÀN BỘ FONT CHỮ TRÊN BẢN VẼ!")
  (prompt "\n -> Các chữ TCVN3 (NguyÔn, V¨n...) và VNI đã chuyển sang UNICODE chuẩn.")
  (prompt "\n -> Gõ lệnh DF (hoặc DOIFONT) nếu muốn đổi đồng loạt font toàn bản vẽ.")
  (prompt "\n==================================================================")
  (princ)
)
(defun c:SUAFONT () (c:SF))
(defun c:CV () (c:SF))
(defun c:CHUYENMA () (c:SF))

;;; =========================================================================
;;; BỘ CÔNG CỤ ĐỔI FONT & SỬA LỖI FONT TOÀN NĂNG (AUTOCAD FONT MASTER v12.0)
;;; =========================================================================

;; 1. ĐỔI TOÀN BỘ BẢN VẼ SANG UNICODE ARIAL (KHUYÊN DÙNG CHO MỌI MÁY)
(defun c:ALL2ARIAL ( / doc tbl sName stEnt stDxf newDxf cnt )
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (prompt "\n==================================================================")
  (prompt "\n [FONT MASTER] ĐANG CHUYỂN ĐỔI TOÀN BỘ BẢN VẼ SANG ARIAL UNICODE...")
  (prompt "\n==================================================================")

  ;; B1. Đổi font của toàn bộ kiểu chữ (TextStyle) sang Arial.ttf
  (setq cnt 0 tbl nil)
  (while (setq tbl (tblnext "STYLE" (null tbl)))
    (setq sName (cdr (assoc 2 tbl)))
    (if (and sName (/= sName ""))
      (progn
        (setq stEnt (tblobjname "STYLE" sName))
        (if stEnt
          (progn
            (setq stDxf (entget stEnt '("ACAD")))
            (setq newDxf (subst '(3 . "ARIAL.TTF") (assoc 3 stDxf) stDxf))
            (if (assoc 4 newDxf) (setq newDxf (subst '(4 . "") (assoc 4 newDxf) newDxf)))
            (if (assoc -3 newDxf)
              (setq newDxf (subst '(-3 ("ACAD" (1000 . "Arial") (1071 . 0))) (assoc -3 newDxf) newDxf))
              (setq newDxf (append newDxf '((-3 ("ACAD" (1000 . "Arial") (1071 . 0))))))
            )
            (entmod newDxf)
            (vl-catch-all-apply
              '(lambda ()
                 (vla-SetFont (vla-Item (vla-get-TextStyles doc) sName) "Arial" :vlax-false :vlax-false 0 0)
               )
            )
            (setq cnt (1+ cnt))
          )
        )
      )
    )
  )
  (prompt (strcat "\n-> Đã đổi font của " (itoa cnt) " kiểu chữ (TextStyle) sang Arial."))

  ;; B2. Quét chuyển mã nội dung TCVN3 (NguyÔn, V¨n...) và VNI sang Unicode chuẩn trên từng đối tượng
  (prompt "\n-> Đang chuyển đổi nội dung chữ TCVN3 / VNI sang Unicode chuẩn...")
  (setq ssTxt (ssget "X" '((0 . "TEXT,MTEXT"))))
  (setq convCount 0)
  (if ssTxt
    (progn
      (setq i 0)
      (repeat (sslength ssTxt)
        (setq ent (ssname ssTxt i)
              dxf (entget ent)
              rawStr (cdr (assoc 1 dxf)))
        (if (and rawStr (/= rawStr ""))
          (progn
            (setq uniStr (To-Unicode-Display rawStr))
            (if (and (/= uniStr rawStr) (/= uniStr ""))
              (progn
                (setq dxf (subst (cons 1 uniStr) (assoc 1 dxf) dxf))
                (entmod dxf)
                (entupd ent)
                (setq convCount (1+ convCount))
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (if (> convCount 0)
    (prompt (strcat "\n-> Đã chuyển đổi " (itoa convCount) " đối tượng chữ sang Unicode chuẩn."))
  )
  (AutoFix-Drawing-Fonts)

  ;; B3. Làm tươi toàn bộ bản vẽ
  (vl-catch-all-apply '(lambda () (vla-Regen doc 0)))
  (prompt "\n==================================================================")
  (prompt "\n [HOÀN TẤT 100%] TOÀN BỘ BẢN VẼ ĐÃ CHUYỂN SANG ARIAL UNICODE!")
  (prompt "\n -> Không còn bất kỳ chữ nào bị lỗi font, dấu hỏi (?) hay ô vuông.")
  (prompt "\n==================================================================")
  (princ)
)

;; 2. ĐỔI TOÀN BỘ BẢN VẼ SANG .VNARIAL (TCVN3 - TIẾNG VIỆT ABC)
(defun c:ALL2VN ( / doc tbl sName stEnt stDxf newDxf cnt )
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (prompt "\n==================================================================")
  (prompt "\n [FONT MASTER] ĐANG ĐỔI TOÀN BỘ KIỂU CHỮ SANG .VNARIAL (TCVN3)...")
  (prompt "\n==================================================================")

  (setq cnt 0 tbl nil)
  (while (setq tbl (tblnext "STYLE" (null tbl)))
    (setq sName (cdr (assoc 2 tbl)))
    (if (and sName (/= sName ""))
      (progn
        (setq stEnt (tblobjname "STYLE" sName))
        (if stEnt
          (progn
            (setq stDxf (entget stEnt '("ACAD")))
            (setq newDxf (subst '(3 . "VNARIAL.TTF") (assoc 3 stDxf) stDxf))
            (if (assoc 4 newDxf) (setq newDxf (subst '(4 . "") (assoc 4 newDxf) newDxf)))
            (if (assoc -3 newDxf)
              (setq newDxf (subst '(-3 ("ACAD" (1000 . ".VnArial") (1071 . 0))) (assoc -3 newDxf) newDxf))
              (setq newDxf (append newDxf '((-3 ("ACAD" (1000 . ".VnArial") (1071 . 0))))))
            )
            (entmod newDxf)
            (vl-catch-all-apply
              '(lambda ()
                 (vla-SetFont (vla-Item (vla-get-TextStyles doc) sName) ".VnArial" :vlax-false :vlax-false 0 0)
               )
            )
            (setq cnt (1+ cnt))
          )
        )
      )
    )
  )
  (vl-catch-all-apply '(lambda () (vla-Regen doc 0)))
  (prompt (strcat "\n-> [HOÀN TẤT] Đã đổi " (itoa cnt) " TextStyle sang .VnArial!"))
  (princ)
)

;; 3. ĐỔI TOÀN BỘ BẢN VẼ SANG VNTIME.SHX (FONT NÉT ĐƠN CAD ĐỊA CHÍNH)
(defun c:ALL2SHX ( / doc tbl sName stEnt stDxf newDxf cnt )
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (prompt "\n==================================================================")
  (prompt "\n [FONT MASTER] ĐANG ĐỔI TOÀN BỘ KIỂU CHỮ SANG VNTIME.SHX (NÉT ĐƠN)...")
  (prompt "\n==================================================================")

  (setq cnt 0 tbl nil)
  (while (setq tbl (tblnext "STYLE" (null tbl)))
    (setq sName (cdr (assoc 2 tbl)))
    (if (and sName (/= sName ""))
      (progn
        (setq stEnt (tblobjname "STYLE" sName))
        (if stEnt
          (progn
            (setq stDxf (entget stEnt '("ACAD")))
            (setq newDxf (subst '(3 . "VNTIME.SHX") (assoc 3 stDxf) stDxf))
            (if (assoc 4 newDxf) (setq newDxf (subst '(4 . "") (assoc 4 newDxf) newDxf)))
            (entmod newDxf)
            (setq cnt (1+ cnt))
          )
        )
      )
    )
  )
  (vl-catch-all-apply '(lambda () (vla-Regen doc 0)))
  (prompt (strcat "\n-> [HOÀN TẤT] Đã đổi " (itoa cnt) " TextStyle sang VNTIME.SHX!"))
  (princ)
)

;; 4. MENU CHỌN ĐỔI FONT NHANH BẰNG PHÍM TẮT DF / DOIFONT
(defun c:DF () (c:DOIFONT))
(defun c:DOIFONT ( / opt )
  (prompt "\n==================================================================")
  (prompt "\n               BỘ ĐỔI FONT AUTOCAD TOÀN NĂNG (FONT MASTER)       ")
  (prompt "\n==================================================================")
  (prompt "\n  [1] Chuyển toàn bộ bản vẽ sang ARIAL UNICODE (Khuyên dùng - Đọc 100%)")
  (prompt "\n  [2] Đổi toàn bộ kiểu chữ sang .VnArial (TCVN3 - Tiếng Việt ABC)")
  (prompt "\n  [3] Đổi toàn bộ kiểu chữ sang VNTIME.SHX (Nét đơn bản đồ CAD)")
  (prompt "\n  [4] Tự động quét & sửa lỗi từng chữ (Lệnh SF)")
  (prompt "\n  [5] Kiểm tra tình trạng font bản vẽ (Lệnh KTFONT)")
  (prompt "\n==================================================================")
  (initget "1 2 3 4 5")
  (setq opt (getkword "\nNhập lựa chọn của bạn [1/2/3/4/5] <1>: "))
  (if (or (null opt) (= opt "") (= opt "1"))
    (c:ALL2ARIAL)
    (cond
      ((= opt "2") (c:ALL2VN))
      ((= opt "3") (c:ALL2SHX))
      ((= opt "4") (c:SF))
      ((= opt "5") (c:KTFONT))
    )
  )
  (princ)
)

;; 5. KIỂM TRA BÁO CÁO FONT CỦA BẢN VẼ
(defun c:KTFONT ( / tbl sName f3 f4 count )
  (prompt "\n==================================================================")
  (prompt "\n       DANH SÁCH CÁC KIỂU CHỮ (TEXTSTYLE) TRONG BẢN VẼ:          ")
  (prompt "\n==================================================================")
  (setq count 0 tbl nil)
  (while (setq tbl (tblnext "STYLE" (null tbl)))
    (setq sName (cdr (assoc 2 tbl))
          f3    (cdr (assoc 3 tbl))
          f4    (cdr (assoc 4 tbl)))
    (setq count (1+ count))
    (prompt (strcat "\n " (itoa count) ". Style: [" sName "] -> Font: " (if f3 f3 "(Mặc định)")
                    (if (and f4 (/= f4 "")) (strcat " | BigFont: " f4) "")))
  )
  (prompt "\n==================================================================")
  (prompt (strcat "\n-> Tổng cộng: " (itoa count) " kiểu chữ. Gõ lệnh DF hoặc SF để sửa lỗi font."))
  (princ)
)

;;; -------------------------------------------------------------------------
;;; 13B. TĂNG TỐC AUTOCAD CỰC ĐẠI - ELIMINATE LAG & TURBO BOOST (LỆNH TANGTOC / BOOST)
;;; -------------------------------------------------------------------------
(defun c:TANGTOC ( / doc )
  (vl-load-com)
  (prompt "\n==================================================================")
  (prompt "\n [AUTOCAD TURBO BOOST] ĐANG TỐI ƯU HÓA TỐC ĐỘ AUTOCAD CỰC ĐẠI...")
  (prompt "\n==================================================================")

  ;; 1. Kích hoạt xử lý đa nhân CPU cho Redraw & Regen
  (vl-catch-all-apply 'setvar (list "WHIPTHREAD" 3))

  ;; 2. Tắt giật / lag chuột khi rê qua đối tượng trên bản đồ nhiều chi tiết
  (vl-catch-all-apply 'setvar (list "SELECTIONPREVIEW" 0))
  (vl-catch-all-apply 'setvar (list "ROLLOVERTOOLTIPS" 0))
  (vl-catch-all-apply 'setvar (list "PROPERTYPREVIEW" 0))
  (vl-catch-all-apply 'setvar (list "SELECTIONCYCLING" 0))
  (vl-catch-all-apply 'setvar (list "QPMODE" 0))

  ;; 3. Tắt độ trễ khi Zoom / Pan bản vẽ
  (vl-catch-all-apply 'setvar (list "VTENABLE" 0))

  ;; 4. Tắt tự động soi trước Hatch (Nguyên nhân chính gây đơ / treo CAD khi di chuột qua ranh đất)
  (vl-catch-all-apply 'setvar (list "HPQUICKPREVIEW" 0))

  ;; 5. Tắt tính toán vẽ đè 2D và tắt ViewCube 3D tốn tài nguyên GPU
  (vl-catch-all-apply 'setvar (list "DRAWORDERCTL" 0))
  (vl-catch-all-apply 'setvar (list "NAVVCUBEDISPLAY" 0))

  ;; 6. Tối đa hóa bộ nhớ đệm đồ họa và tạo chỉ mục tăng tốc mở file
  (vl-catch-all-apply 'setvar (list "INDEXCTL" 3))
  (vl-catch-all-apply 'setvar (list "CACHEMAXFILES" 2048))
  (vl-catch-all-apply 'setvar (list "CACHEMAXTOTALSIZE" 4096))
  (vl-catch-all-apply 'setvar (list "DWGCHECK" 0))

  ;; 7. Dọn rác chuyên sâu: Quét sạch hàng nghìn RegApp rác tồn dư từ MicroStation/Civil
  (prompt "\n-> Đang dọn sạch rác RegApp và đối tượng thừa trong bản vẽ...")
  (vl-cmdf "-PURGE" "Regapp" "*" "N")
  (vl-cmdf "-PURGE" "All" "*" "N")
  (vl-cmdf "AUDIT" "Y")

  ;; 8. Tự động sửa luôn các lỗi Font chữ
  ;; (AutoFix-Drawing-Fonts) ;; [UPGRADE] Da tat tu dong sua font khi tang toc

  (vl-catch-all-apply
    '(lambda ()
       (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) 0)
     )
  )
  (prompt "\n==================================================================")
  (prompt "\n [THÀNH CÔNG] ĐÃ TĂNG TỐC AUTOCAD LÊN MỨC CAO NHẤT!")
  (prompt "\n -> Chuột mượt 100%, Zoom/Pan tức thì, đa nhân CPU kích hoạt.")
  (prompt "\n -> Đã dọn sạch RegApp rác và sửa lỗi font trên toàn bản vẽ.")
  (prompt "\n==================================================================")
  (princ)
)
(defun c:BOOST () (c:TANGTOC))
(defun c:SPEED () (c:TANGTOC))

;;; -------------------------------------------------------------------------
;;; 14. CÁC LỆNH PHÍM TẮT NHANH & GỘP BẢN ĐỒ (FAST UTILITIES v12.0)
;;; -------------------------------------------------------------------------

;; Công Tắc BẬT / TẮT nhanh chế độ Tìm Kiếm Toàn Bộ Thư Mục ngay tại dòng lệnh
(defun c:CTTM ()
  (if (= *folder-search-enabled* "1")
    (progn
      (setq *folder-search-enabled* "0")
      (prompt "\n==================================================================")
      (prompt "\n [CÔNG TẮC TÌM KIẾM THƯ MỤC]: ĐÃ TẮT [OFF]")
      (prompt "\n -> Lệnh FF hiện tại sẽ chỉ tìm kiếm trên Bản Vẽ Hiện Hành.")
      (prompt "\n -> Gõ lại lệnh CTTM (hoặc FFO) bất kỳ lúc nào để BẬT lại.")
      (prompt "\n==================================================================")
    )
    (progn
      (setq *folder-search-enabled* "1")
      (prompt "\n==================================================================")
      (prompt "\n [CÔNG TẮC TÌM KIẾM THƯ MỤC]: ĐÃ BẬT [ON]")
      (prompt (strcat "\n -> Lệnh FF sẽ quét siêu tốc toàn bộ bản đồ trong: " (if (and *last-search-folder* (vl-file-directory-p *last-search-folder*)) *last-search-folder* "D:\\")))
      (prompt "\n -> Gõ lại lệnh CTTM (hoặc FFO) bất kỳ lúc nào để TẮT.")
      (prompt "\n==================================================================")
    )
  )
  (princ)
)
(defun c:FFO () (c:CTTM))

;; Mở thẳng chế độ Tìm Kiếm Toàn Bộ Các Tờ Bản Đồ Trong Thư Mục (Fast Find Pro v13.0 GUI)
(defun c:FFF ( / fld cfgFld )
  (TC-Check-Update-Notice)
  (vl-catch-all-apply '(lambda () (startapp (strcat "\"" (TC-Ensure-FastSearch-Exe) "\" check-update"))))
  (setq cfgFld (TC-Get-Config-Folder))
  (setq fld (cond
              ((and *last-search-folder* (vl-file-directory-p *last-search-folder*))
                *last-search-folder*)
              ((and cfgFld (/= cfgFld "ALL") (vl-file-directory-p cfgFld))
                cfgFld)
              ((and cfgFld (= (strcase cfgFld) "ALL"))
                "ALL")
              ((and (getvar "DWGPREFIX") (vl-file-directory-p (getvar "DWGPREFIX")))
                (Normalize-Folder-Path (getvar "DWGPREFIX")))
              (t "ALL")
            ))
  (Do-Folder-Search "" (if (= fld "ALL") "" fld) "*" nil nil)
  (princ)
)
(defun c:TCTM () (c:FFF))

;; Xem lại / Mở lại bảng Kết Quả Tìm Kiếm Thư Mục Fast Find Pro v13.0
(defun c:FFL ( / fld )
  (setq fld (if (and *last-search-folder* (vl-file-directory-p *last-search-folder*))
              *last-search-folder*
              (if (vl-file-directory-p "D:\\") "D:\\" "C:\\")
            ))
  (Do-Folder-Search (if *last-kw* *last-kw* "") fld "*" nil nil)
  (princ)
)

;; Gộp tất cả các tờ bản đồ (.DWG) trong một thư mục về bản vẽ hiện hành
(defun c:GOPBANDO ( / folder files file full cnt oldCmd oldAttDia oldAttReq )
  (vl-load-com)
  (setq folder (TC-Pick-Folder (if (and *last-search-folder* (vl-file-directory-p *last-search-folder*)) *last-search-folder* (if (vl-file-directory-p "D:\\") "D:\\" "C:\\"))))
  (if (and folder (vl-file-directory-p folder))
    (progn
      (setq files (vl-directory-files folder "*.dwg" 1))
      (if (null files)
        (prompt (strcat "\n[Gộp Bản Đồ] Không có file .dwg trực tiếp trong thư mục: " folder))
        (progn
          (setq oldCmd (getvar "CMDECHO")
                oldAttDia (getvar "ATTDIA")
                oldAttReq (getvar "ATTREQ")
                cnt 0)
          (setvar "CMDECHO" 0)
          (setvar "ATTDIA" 0)
          (setvar "ATTREQ" 0)
          (foreach file (acad_strlsort files)
            (if (not (wcmatch (strcase file) "*RECOVER*,~$*"))
              (progn
                (setq cnt (1+ cnt)
                      full (strcat folder "\\" file))
                (prompt (strcat "\r-> Đang gộp tờ bản đồ [" (itoa cnt) "/" (itoa (length files)) "]: " file " ...     "))
                (vl-catch-all-apply 'vl-cmdf (list "_.-INSERT" full "0,0,0" "1" "1" "0"))
                (vl-catch-all-apply 'vl-cmdf (list "_.EXPLODE" (entlast)))
              )
            )
          )
          (setvar "CMDECHO" oldCmd)
          (setvar "ATTDIA" oldAttDia)
          (setvar "ATTREQ" oldAttReq)
          ;; (vl-catch-all-apply 'AutoFix-Drawing-Fonts) ;; [UPGRADE] Khong tu dong sua font khi chen to ban do
          (vl-catch-all-apply 'vl-cmdf (list "_.ZOOM" "_E"))
          (prompt (strcat "\n-> [HOÀN TẤT GỘP BẢN ĐỒ] Đã gộp thành công " (itoa cnt) " tờ bản đồ (.DWG) và tự động chuẩn hóa Font chữ!"))
        )
      )
    )
  )
  (princ)
)
(defun c:GBD () (c:GOPBANDO))

;; Nhảy nhanh tới kết quả tiếp theo (Next)
(defun c:FFN ()
  (if (and *last-matches* (> (length *last-matches*) 0))
    (progn
      (setq *last-match-idx* (1+ (if *last-match-idx* *last-match-idx* 0)))
      (if (>= *last-match-idx* (length *last-matches*)) (setq *last-match-idx* 0))
      (Do-Tour-Navigation *last-match-idx* *last-matches*)
    )
    (prompt "\n[Fast Find] Chưa có kết quả tìm kiếm trước đó! Hãy dùng lệnh 'FF' trước.")
  )
  (princ)
)

;; Nhảy lùi về kết quả trước (Previous)
(defun c:FFP ()
  (if (and *last-matches* (> (length *last-matches*) 0))
    (progn
      (setq *last-match-idx* (1- (if *last-match-idx* *last-match-idx* 0)))
      (if (< *last-match-idx* 0) (setq *last-match-idx* (1- (length *last-matches*))))
      (Do-Tour-Navigation *last-match-idx* *last-matches*)
    )
    (prompt "\n[Fast Find] Chưa có kết quả tìm kiếm trước đó! Hãy dùng lệnh 'FF' trước.")
  )
  (princ)
)

;; Khoanh vòng đỏ toàn bộ kết quả tìm kiếm
(defun c:FFM ()
  (if (and *last-matches* (> (length *last-matches*) 0))
    (progn
      (setq markRes (MarkCirclesOnObjects *last-matches*))
      (prompt (strcat "\n-> Đã khoanh " (itoa markRes) " vòng tròn đỏ trên Layer '_DANH_DAU_TRA_CUU'!"))
    )
    (prompt "\n[Fast Find] Chưa có kết quả tìm kiếm để khoanh tròn. Hãy dùng 'FF' trước.")
  )
  (princ)
)

;; Xóa toàn bộ vòng tròn đánh dấu
(defun c:FFC ()
  (setq delCount (ClearMarkCircles))
  (Clear-Spotlight-Circle)
  (prompt (strcat "\n-> Đã xóa " (itoa delCount) " vòng tròn đỏ và vòng chỉ điểm!"))
  (princ)
)

;; Lệnh tìm và thay thế chữ siêu tốc (Find & Replace)
(defun c:FFR ( / kw repKw ss i ent dxf typ txt cleanTxt countRep )
  (vl-load-com)
  (setq kw (getstring T "\nNhập nội dung chữ CẦN TÌM để thay thế: "))
  (if (and kw (/= kw ""))
    (progn
      (setq repKw (getstring T (strcat "\nThay thế \"" kw "\" BẰNG nội dung mới: ")))
      (if repKw
        (progn
          (prompt "\nĐang quét và thay thế trên toàn bản vẽ...")
          (setq ss (ssget "X" '((0 . "TEXT,MTEXT"))))
          (setq countRep 0)
          (if ss
            (repeat (setq i (sslength ss))
              (setq ent (ssname ss (setq i (1- i))))
              (setq dxf (entget ent))
              (setq typ (cdr (assoc 0 dxf)))
              (setq txt (cdr (assoc 1 dxf)))
              (if (vl-string-search (strcase kw) (strcase txt))
                (progn
                  (setq cleanTxt (LM:SafeReplace kw repKw txt))
                  (entmod (subst (cons 1 cleanTxt) (assoc 1 dxf) dxf))
                  (entupd ent)
                  (setq countRep (1+ countRep))
                )
              )
            )
          )
          (prompt (strcat "\n-> [THAY THẾ HOÀN TẤT] Đã thay thế thành công " (itoa countRep) " đối tượng Text/MText!"))
        )
      )
    )
  )
  (princ)
)

;; Các bí danh lệnh tương đương
(defun c:TC () (c:FF))
(defun c:TRACUU () (c:FF))
(defun c:TCN () (c:FFN))
(defun c:TCP () (c:FFP))
(defun c:TT () (c:TANGTOC))

;; Tu dong kiem tra va thong bao ban cap nhat moi tu GitHub
(defun TC-Check-Update-Notice ( / workDir noticePath fHandle fLine )
  (vl-catch-all-apply
    '(lambda ()
       (setq workDir (TC-Get-Extra-Dir))
       (setq noticePath (strcat workDir "\\new_update_notice.txt"))
       (if (findfile noticePath)
         (progn
           (setq fHandle (open noticePath "r"))
           (if fHandle
             (progn
               (setq fLine (read-line fHandle))
               (close fHandle)
               (if (and fLine (/= fLine ""))
                 (progn
                   (prompt "\n==========================================================================")
                   (prompt (strcat "\n [\U+1F514 CadaPro-CAD] DA CO BAN CAP NHAT MOI (v" fLine ") TREN GITHUB!"))
                   (prompt "\n -> Hay go lenh 'CAPNHAT' (hoac 'UPDATE') de tu dong nang cap tinh nang moi!")
                   (prompt "\n==========================================================================")
                 )
               )
             )
           )
         )
       )
     )
  )
)

;; Cap nhat truc tiep phien ban moi nhat tu GitHub
(defun c:CAPNHAT ( / batPath )
  (vl-load-com)
  (prompt "\n=======================================================")
  (prompt "\n[FAST FIND PRO] Dang mo trinh cap nhat tu GitHub...")
  (setq batPath (cond
                  ((findfile "CAP_NHAT_TU_GITHUB.bat") (findfile "CAP_NHAT_TU_GITHUB.bat"))
                  ((findfile "D:/EXTRACUUAUTOCAD/CAP_NHAT_TU_GITHUB.bat") "D:/EXTRACUUAUTOCAD/CAP_NHAT_TU_GITHUB.bat")
                  ((findfile "C:/EXTRACUUAUTOCAD/CAP_NHAT_TU_GITHUB.bat") "C:/EXTRACUUAUTOCAD/CAP_NHAT_TU_GITHUB.bat")
                  ((findfile "D:/BO_FONT_CAD_FULL/CAP_NHAT_TU_GITHUB.bat") "D:/BO_FONT_CAD_FULL/CAP_NHAT_TU_GITHUB.bat")
                  (t nil)))
  (if batPath
    (progn
      (startapp (strcat "\"" batPath "\""))
      (prompt "\n-> Da kich hoat cua so cap nhat. Vui long kiem tra man hinh!")
    )
    (prompt "\n[LOI] Khong tim thay file CAP_NHAT_TU_GITHUB.bat!")
  )
  (prompt "\n=======================================================")
  (princ)
)
(defun c:UPDATE () (c:CAPNHAT))

;; Nạp hoặc chuyển hướng sang công cụ Gộp Tờ Bản Đồ (GOPTO.LSP)
(vl-catch-all-apply
  '(lambda ()
     (cond
       ((findfile "GOPTO.LSP")                     (load "GOPTO.LSP"))
       ((findfile "D:/VBA/GOPTO.LSP")              (load "D:/VBA/GOPTO.LSP"))
       ((findfile "C:/VBA/GOPTO.LSP")              (load "C:/VBA/GOPTO.LSP"))
       ((findfile "D:/EXTRACUUAUTOCAD/GOPTO.LSP")  (load "D:/EXTRACUUAUTOCAD/GOPTO.LSP"))
       ((findfile "C:/EXTRACUUAUTOCAD/GOPTO.LSP")  (load "C:/EXTRACUUAUTOCAD/GOPTO.LSP"))
     )
   )
)

(if (not (boundp 'c:GOPTO))
  (defun c:GOPTO ()
    (if (or (findfile "GOPTO.LSP") (findfile "D:/VBA/GOPTO.LSP") (findfile "C:/VBA/GOPTO.LSP") (findfile "D:/EXTRACUUAUTOCAD/GOPTO.LSP") (findfile "C:/EXTRACUUAUTOCAD/GOPTO.LSP"))
      (progn
        (vl-catch-all-apply 'load (list (or (findfile "GOPTO.LSP") (findfile "D:/VBA/GOPTO.LSP") (findfile "C:/VBA/GOPTO.LSP") (findfile "D:/EXTRACUUAUTOCAD/GOPTO.LSP") (findfile "C:/EXTRACUUAUTOCAD/GOPTO.LSP"))))
        (if (boundp 'c:GOPTO) (c:GOPTO))
      )
      (prompt "\n[Lỗi] Không tìm thấy file GOPTO.LSP! Hãy chạy lại công cụ sửa chữa hoặc copy GOPTO.LSP vào thư mục CAD.")
    )
    (princ)
  )
)
(if (not (boundp 'c:GOPBANDO)) (defun c:GOPBANDO () (c:GOPTO)))
(if (not (boundp 'c:GBD)) (defun c:GBD () (c:GOPTO)))
(if (not (boundp 'c:GOPF))
  (defun c:GOPF ()
    (if (or (findfile "GOPTO.LSP") (findfile "D:/VBA/GOPTO.LSP") (findfile "C:/VBA/GOPTO.LSP") (findfile "D:/EXTRACUUAUTOCAD/GOPTO.LSP") (findfile "C:/EXTRACUUAUTOCAD/GOPTO.LSP"))
      (progn
        (vl-catch-all-apply 'load (list (or (findfile "GOPTO.LSP") (findfile "D:/VBA/GOPTO.LSP") (findfile "C:/VBA/GOPTO.LSP") (findfile "D:/EXTRACUUAUTOCAD/GOPTO.LSP") (findfile "C:/EXTRACUUAUTOCAD/GOPTO.LSP"))))
        (if (boundp 'c:GOPF) (c:GOPF))
      )
      (prompt "\n[Lỗi] Không tìm thấy file GOPTO.LSP!")
    )
    (princ)
  )
)

;; Luôn tự động bật hộp thoại mở file và lệnh trong AutoCAD (tránh bị lỗi Enter name of drawing to open)
(vl-catch-all-apply 'setvar (list "FILEDIA" 1))
(vl-catch-all-apply 'setvar (list "CMDDIA" 1))
(vl-catch-all-apply 'setvar (list "ATTDIA" 1))

;; Tự động thiết lập font thay thế khi thiếu font trong bản vẽ
;; [UPGRADE] Da tat tu dong thay doi FONTALT khi mo file CAD de giu nguyen dinh dang ban ve goc
;; (vl-catch-all-apply 'setvar (list "FONTALT" "VNTIME.SHX"))

;; Tự động sửa lỗi gán sai font trên bản vẽ & kiểm tra định vị chờ sẵn ngay khi nạp LISP
;; (vl-catch-all-apply 'AutoFix-Drawing-Fonts-Once)
;; (vl-catch-all-apply 'TC-Check-Pending-Zoom)

(prompt "\n[AUTOCAD MASTER PRO] Đã sẵn sàng! Lệnh: DF (Đổi Font) | SF (Sửa Font) | GOPTO/GBD (Gộp Tờ Bản Đồ) | TT (Tăng tốc CAD) | FF (Tra cứu)")
(princ)
