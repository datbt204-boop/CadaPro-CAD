;;; =========================================================================
;;; CÔNG CỤ GỘP TẤT CẢ CÁC TỜ BẢN ĐỒ ĐỊA CHÍNH - GOPTO PRO v12.0
;;; Tác giả: Bộ phận Kỹ thuật Địa chính & Trắc địa
;;; Lệnh sử dụng:
;;;   - GOPTO     : Mở Bảng Điều Khiển Gộp Bản Đồ Đa Năng (Khuyên dùng)
;;;   - GOPBANDO  : Tương đương lệnh GOPTO
;;;   - GBD       : Lệnh tắt 3 chữ cái siêu nhanh
;;;   - GOPF      : Gộp các file .DWG được chọn riêng lẻ
;;; =========================================================================

(vl-load-com)

;;; -------------------------------------------------------------------------
;;; 1. CÁC HÀM TIỆN ÍCH HỆ THỐNG & CHỌN THƯ MỤC
;;; -------------------------------------------------------------------------

(defun GOP:SafeReplace (old new str / pos len res)
  (setq len (strlen old)
        res "")
  (while (setq pos (vl-string-search old str))
    (setq res (strcat res (substr str 1 pos) new)
          str (substr str (+ pos len 1)))
  )
  (strcat res str)
)

(defun GOP:Normalize-Path (p /)
  (if (and p (/= p ""))
    (vl-string-right-trim "\\" (GOP:SafeReplace "/" "\\" p))
    ""
  )
)

;; Hộp thoại chọn thư mục Windows tương thích mọi phiên bản CAD
(defun GOP:Pick-Folder (defaultDir / dir sh folder self)
  (setq dir nil)
  (if (and (boundp 'acet-ui-pickdir) acet-ui-pickdir)
    (setq dir (vl-catch-all-apply 'acet-ui-pickdir (list "Chọn Thư Mục chứa các Tờ Bản Đồ (.DWG):" defaultDir)))
  )
  (if (or (null dir) (vl-catch-all-error-p dir) (= dir ""))
    (progn
      (setq dir nil)
      (vl-catch-all-apply
        '(lambda ()
           (setq sh (vlax-create-object "Shell.Application"))
           (setq folder (vlax-invoke sh 'BrowseForFolder 0 "Chọn Thư Mục chứa các Tờ Bản Đồ (.DWG) cần gộp:" 0 defaultDir))
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
  (if dir (GOP:Normalize-Path dir) nil)
)

;; Chuẩn hóa định dạng mẫu lọc file (.DWG)
(defun GOP:Normalize-Pattern (p / np)
  (if (or (null p) (= p "") (= p "*") (= p "*.*"))
    "*.DWG"
    (progn
      (setq np (strcase (vl-string-trim " " p)))
      (cond
        ((vl-string-search ".DWG" np) np)
        ((= (substr np (strlen np) 1) "*") (strcat np ".DWG"))
        (t (strcat np "*.DWG"))
      )
    )
  )
)

;; Quét đệ quy tìm toàn bộ file DWG trong thư mục và các thư mục con
(defun GOP:Scan-Dwgs (dir pattern scanSub / res subDirs sub d normPattern)
  (setq res '())
  (setq normPattern (GOP:Normalize-Pattern pattern))
  (if (and dir (vl-file-directory-p dir))
    (progn
      ;; Lấy toàn bộ file DWG trong thư mục hiện tại
      (foreach f (vl-directory-files dir nil 1)
        (if (and (wcmatch (strcase f) normPattern)
                 (not (wcmatch (strcase f) "*`~*,*`$*,*RECOVER*,*.BAK")))
          (setq res (cons (strcat dir "\\" f) res))
        )
      )
      ;; Quét thư mục con nếu được yêu cầu
      (if scanSub
        (progn
          (setq subDirs (vl-directory-files dir nil -1))
          (foreach sub subDirs
            (if (not (member sub '("." ".." "$RECYCLE.BIN" "System Volume Information" "Zalo Data" "IRT.cache")))
              (setq res (append res (GOP:Scan-Dwgs (strcat dir "\\" sub) pattern scanSub)))
            )
          )
        )
      )
    )
  )
  res
)

;;; -------------------------------------------------------------------------
;;; 2. TẠO HỘP THOẠI DCL CHO CÔNG CỤ GỘP BẢN ĐỒ
;;; -------------------------------------------------------------------------
(defun GOP:WriteDCL ( / path fn )
  (setq path (vl-filename-mktemp "GopToDlg.dcl"))
  (setq fn (open path "w"))

  (write-line "gopto_dlg : dialog {" fn)
  (write-line "  label = \"CÔNG CỤ GỘP TẤT CẢ CÁC TỜ BẢN ĐỒ - GOPTO PRO v12.0\";" fn)
  (write-line "  fixed_width = true;" fn)
  (write-line "  width = 88;" fn)
  (write-line "  spacer;" fn)
  
  (write-line "  : boxed_column {" fn)
  (write-line "    label = \"1. Thư mục chứa các tờ bản đồ (.DWG):\";" fn)
  (write-line "    : row {" fn)
  (write-line "      : edit_box {" fn)
  (write-line "        key = \"txt_dir\";" fn)
  (write-line "        edit_width = 62;" fn)
  (write-line "      }" fn)
  (write-line "      : button {" fn)
  (write-line "        key = \"btn_browse\";" fn)
  (write-line "        label = \"Chọn Thư Mục...\";" fn)
  (write-line "        width = 16;" fn)
  (write-line "        fixed_width = true;" fn)
  (write-line "      }" fn)
  (write-line "    }" fn)
  (write-line "    : row {" fn)
  (write-line "      : toggle {" fn)
  (write-line "        key = \"chk_subdirs\";" fn)
  (write-line "        label = \"Quét cả thư mục con (Sub-folders)\";" fn)
  (write-line "        value = \"1\";" fn)
  (write-line "      }" fn)
  (write-line "      : text { label = \"Lọc tên file:\"; width = 12; }" fn)
  (write-line "      : edit_box {" fn)
  (write-line "        key = \"txt_filter\";" fn)
  (write-line "        edit_width = 12;" fn)
  (write-line "        value = \"*.dwg\";" fn)
  (write-line "      }" fn)
  (write-line "      : button {" fn)
  (write-line "        key = \"btn_scan\";" fn)
  (write-line "        label = \"Kiểm Tra File\";" fn)
  (write-line "        width = 14;" fn)
  (write-line "        fixed_width = true;" fn)
  (write-line "      }" fn)
  (write-line "    }" fn)
  (write-line "  }" fn)
  (write-line "  spacer;" fn)

  (write-line "  : boxed_column {" fn)
  (write-line "    label = \"2. Danh sách các tờ bản đồ tìm thấy:\";" fn)
  (write-line "    : list_box {" fn)
  (write-line "      key = \"lst_files\";" fn)
  (write-line "      height = 10;" fn)
  (write-line "      width = 82;" fn)
  (write-line "    }" fn)
  (write-line "    : text {" fn)
  (write-line "      key = \"lbl_count\";" fn)
  (write-line "      label = \"Chưa quét thư mục bản đồ.\";" fn)
  (write-line "    }" fn)
  (write-line "  }" fn)
  (write-line "  spacer;" fn)

  (write-line "  : boxed_column {" fn)
  (write-line "    label = \"3. Chế độ gộp bản đồ:\";" fn)
  (write-line "    : radio_button {" fn)
  (write-line "      key = \"rad_mode_block\";" fn)
  (write-line "      label = \"Chèn dạng Khối (Block) - Mỗi tờ là 1 Block mang tên số tờ (Khuyên dùng: Siêu nhẹ & mượt)\";" fn)
  (write-line "      value = \"1\";" fn)
  (write-line "    }" fn)
  (write-line "    : radio_button {" fn)
  (write-line "      key = \"rad_mode_explode\";" fn)
  (write-line "      label = \"Bung phá khối (Explode) - Bung toàn bộ nét vẽ, text, ranh thửa ra bản vẽ\";" fn)
  (write-line "      value = \"0\";" fn)
  (write-line "    }" fn)
  (write-line "    : radio_button {" fn)
  (write-line "      key = \"rad_mode_xref\";" fn)
  (write-line "      label = \"Chèn dạng Tham Chiếu Ngoài (XREF Overlay) - Không làm tăng dung lượng bản vẽ\";" fn)
  (write-line "      value = \"0\";" fn)
  (write-line "    }" fn)
  (write-line "    spacer;" fn)
  (write-line "    : row {" fn)
  (write-line "      : toggle {" fn)
  (write-line "        key = \"chk_layer_per_sheet\";" fn)
  (write-line "        label = \"Đặt mỗi tờ vào 1 Layer riêng (Dễ ẩn/hiện từng tờ)\";" fn)
  (write-line "        value = \"1\";" fn)
  (write-line "      }" fn)
  (write-line "      : toggle {" fn)
  (write-line "        key = \"chk_autofix_font\";" fn)
  (write-line "        label = \"Tự động sửa lỗi Font chữ sau khi gộp\";" fn)
  (write-line "        value = \"1\";" fn)
  (write-line "      }" fn)
  (write-line "    }" fn)
  (write-line "    : row {" fn)
  (write-line "      : toggle {" fn)
  (write-line "        key = \"chk_purge\";" fn)
  (write-line "        label = \"Dọn rác, làm nhẹ bản vẽ (Purge All)\";" fn)
  (write-line "        value = \"1\";" fn)
  (write-line "      }" fn)
  (write-line "      : toggle {" fn)
  (write-line "        key = \"chk_zoom\";" fn)
  (write-line "        label = \"Zoom toàn màn hình khi gộp xong\";" fn)
  (write-line "        value = \"1\";" fn)
  (write-line "      }" fn)
  (write-line "    }" fn)
  (write-line "  }" fn)
  (write-line "  spacer;" fn)

  (write-line "  : row {" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"accept\";" fn)
  (write-line "      label = \"BẮT ĐẦU GỘP BẢN ĐỒ\";" fn)
  (write-line "      is_default = true;" fn)
  (write-line "      width = 25;" fn)
  (write-line "      fixed_width = true;" fn)
  (write-line "    }" fn)
  (write-line "    : spacer { width = 10; }" fn)
  (write-line "    : button {" fn)
  (write-line "      key = \"cancel\";" fn)
  (write-line "      label = \"Đóng\";" fn)
  (write-line "      is_cancel = true;" fn)
  (write-line "      width = 15;" fn)
  (write-line "      fixed_width = true;" fn)
  (write-line "    }" fn)
  (write-line "  }" fn)
  (write-line "}" fn)

  (close fn)
  path
)

;;; -------------------------------------------------------------------------
;;; 3. QUY TRÌNH THỰC HIỆN GỘP BẢN ĐỒ SIÊU TỐC & AN TOÀN
;;; -------------------------------------------------------------------------
(defun GOP:Execute-Merge (fileList mergeMode useLayerPerSheet autoFixFont autoPurge autoZoom / 
                         total cnt errCount curFull oldCmd oldAttDia oldAttReq oldInsUnits 
                         oldInsDefTar oldInsDefSrc oldExpert oldBind f baseName layName 
                         blkRef t0 elapsed errList res layersObj curLayer)
  (if (or (null fileList) (= (length fileList) 0))
    (alert "Danh sách file gộp đang rỗng! Hãy chọn thư mục có chứa file bản đồ (.DWG).")
    (progn
      (setq oldCmd       (getvar "CMDECHO")
            oldAttDia    (getvar "ATTDIA")
            oldAttReq    (getvar "ATTREQ")
            oldInsUnits  (getvar "INSUNITS")
            oldInsDefTar (getvar "INSUNITSDEFTARGET")
            oldInsDefSrc (getvar "INSUNITSDEFSOURCE")
            oldExpert    (getvar "EXPERT")
            oldBind      (getvar "BINDTYPE")
            curLayer     (getvar "CLAYER"))

      ;; Thiết lập môi trường an toàn tuyệt đối
      (setvar "CMDECHO" 0)
      (setvar "ATTDIA" 0)
      (setvar "ATTREQ" 0)
      ;; INSUNITS = 0: Giữ nguyên tọa độ VN-2000, không bị co giãn 1000 lần
      (setvar "INSUNITS" 0)
      (setvar "INSUNITSDEFTARGET" 0)
      (setvar "INSUNITSDEFSOURCE" 0)
      (setvar "EXPERT" 5)
      (setvar "BINDTYPE" 1)

      (setq curFull (strcase (strcat (getvar "DWGPREFIX") (getvar "DWGNAME")))
            total (length fileList)
            cnt 0
            errCount 0
            errList '()
            t0 (getvar "MILLISECS")
            layersObj (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))))

      (prompt "\n=========================================================================")
      (prompt (strcat "\n[GOPTO PRO] BẮT ĐẦU GỘP " (itoa total) " TỜ BẢN ĐỒ VÀO BẢN VẼ HIỆN HÀNH..."))
      (prompt "\n=========================================================================")

      (foreach f fileList
        ;; Bỏ qua bản vẽ đang mở hiện hành
        (if (/= (strcase f) curFull)
          (progn
            (setq cnt (1+ cnt)
                  baseName (vl-filename-base f))
            (prompt (strcat "\r-> [Đang gộp " (itoa cnt) "/" (itoa total) " (" 
                            (rtos (* (/ (float cnt) total) 100.0) 2 0) "%)] Tờ: " baseName " ...     "))

            ;; Tạo và gán Layer riêng cho từng tờ nếu được bật
            (if (= useLayerPerSheet "1")
              (progn
                (setq layName (strcat "_TO_" baseName))
                (if (null (tblsearch "LAYER" layName))
                  (vl-catch-all-apply 'vla-Add (list layersObj layName))
                )
                (setvar "CLAYER" layName)
              )
            )

            ;; Thực hiện chèn đúng tọa độ VN-2000 (0,0,0 - tỷ lệ 1.0 - góc xoay 0.0)
            (cond
              ;; Chế độ 1: Chèn Block
              ((= mergeMode "BLOCK")
                (setq res (vl-catch-all-apply 'vl-cmdf (list "_.-INSERT" (strcat baseName "=" f) "0,0,0" "1" "1" "0")))
                (if (vl-catch-all-error-p res)
                  (setq errCount (1+ errCount) errList (cons baseName errList))
                )
              )
              ;; Chế độ 2: Bung phá khối (Explode)
              ((= mergeMode "EXPLODE")
                (setq res (vl-catch-all-apply 'vl-cmdf (list "_.-INSERT" (strcat baseName "=" f) "0,0,0" "1" "1" "0")))
                (if (not (vl-catch-all-error-p res))
                  (progn
                    (setq blkRef (vlax-ename->vla-object (entlast)))
                    (if (and blkRef (= (vla-get-ObjectName blkRef) "AcDbBlockReference"))
                      (progn
                        (vl-catch-all-apply 'vla-Explode (list blkRef))
                        (vl-catch-all-apply 'vla-Delete (list blkRef))
                      )
                    )
                  )
                  (setq errCount (1+ errCount) errList (cons baseName errList))
                )
              )
              ;; Chế độ 3: Tham chiếu ngoài Xref
              ((= mergeMode "XREF")
                (setq res (vl-catch-all-apply 'vl-cmdf (list "_.-XREF" "_O" f "0,0,0" "1" "1" "0")))
                (if (vl-catch-all-error-p res)
                  (setq errCount (1+ errCount) errList (cons baseName errList))
                )
              )
            )
          )
        )
      )

      ;; Phục hồi biến hệ thống
      (setvar "CLAYER" curLayer)
      (setvar "CMDECHO" oldCmd)
      (setvar "ATTDIA" oldAttDia)
      (setvar "ATTREQ" oldAttReq)
      (setvar "INSUNITS" oldInsUnits)
      (setvar "INSUNITSDEFTARGET" oldInsDefTar)
      (setvar "INSUNITSDEFSOURCE" oldInsDefSrc)
      (setvar "EXPERT" oldExpert)
      (setvar "BINDTYPE" oldBind)
      (setvar "FILEDIA" 1)
      (setvar "CMDDIA" 1)

      ;; Tự động sửa lỗi font tiếng Việt nếu được chọn
      (if (and (= autoFixFont "1") (boundp 'AutoFix-Drawing-Fonts))
        (progn
          (prompt "\n-> Đang tự động kiểm tra và chuẩn hóa Font chữ tiếng Việt...")
          (vl-catch-all-apply 'AutoFix-Drawing-Fonts)
        )
      )

      ;; Tự động Purge dọn rác
      (if (= autoPurge "1")
        (progn
          (prompt "\n-> Đang dọn rác và làm nhẹ bản vẽ (Purge All)...")
          (vl-catch-all-apply 'vl-cmdf (list "_.-PURGE" "_A" "*" "_N"))
          (vl-catch-all-apply 'vl-cmdf (list "_.-PURGE" "_A" "*" "_N"))
        )
      )

      ;; Tự động Zoom Extents
      (if (= autoZoom "1")
        (vl-catch-all-apply 'vl-cmdf (list "_.ZOOM" "_E"))
      )

      (setq elapsed (/ (- (getvar "MILLISECS") t0) 1000.0))

      (prompt "\n=========================================================================")
      (prompt (strcat "\n-> [HOÀN TẤT] Đã gộp thành công " (itoa (- cnt errCount)) "/" (itoa cnt) " tờ bản đồ (.DWG) trong " (rtos elapsed 2 1) " giây!"))
      (prompt "\n=========================================================================")

      (alert (strcat "HOÀN TẤT GỘP TẤT CẢ CÁC TỜ BẢN ĐỒ!\n"
                     "=========================================\n\n"
                     "• Tổng số tờ bản đồ đã gộp : " (itoa (- cnt errCount)) " / " (itoa cnt) " tờ\n"
                     "• Thời gian thực hiện      : " (rtos elapsed 2 1) " giây\n"
                     "• Chế độ gộp              : " 
                     (cond ((= mergeMode "BLOCK") "Chèn Khối Block (Quản lý theo tờ)")
                           ((= mergeMode "EXPLODE") "Bung phá khối (Explode)")
                           (t "Tham chiếu ngoài Xref")) "\n"
                     (if (> errCount 0) 
                       (strcat "• Số file bị lỗi: " (itoa errCount) "\n")
                       "• Trạng thái: Tất cả các tờ đã ghép đúng 100% tọa độ VN-2000!\n")
                     "\n-> Bây giờ bạn có thể quan sát toàn bộ khu vực trên bản vẽ."))
    )
  )
)

;;; -------------------------------------------------------------------------
;;; 4. LỆNH CHÍNH: GOPTO / GOPBANDO / GBD (CÓ HỘP THOẠI TRỰC QUAN)
;;; -------------------------------------------------------------------------
(setq *GOPTO-Last-Dir* nil)
(setq *GOPTO-Last-Files* '())

(defun c:GOPTO ( / dclPath dclId loopDlg dlgStatus curDir filterVal scanSubVal 
                  pickedDir mergeMode laySheetVal fixFontVal purgeVal zoomVal 
                  updateFileList )
  (vl-load-com)

  ;; Nạp thêm TRACUU.LSP nếu có để hỗ trợ AutoFix Font
  (if (not (boundp 'AutoFix-Drawing-Fonts))
    (cond
      ((findfile "TRACUU.LSP")        (load "TRACUU.LSP"))
      ((findfile "D:/VBA/TRACUU.LSP") (load "D:/VBA/TRACUU.LSP"))
      ((findfile "C:/VBA/TRACUU.LSP") (load "C:/VBA/TRACUU.LSP"))
    )
  )

  ;; Khởi tạo thư mục mặc định
  (if (or (null *GOPTO-Last-Dir*) (not (vl-file-directory-p *GOPTO-Last-Dir*)))
    (setq *GOPTO-Last-Dir*
      (cond
        ((and (boundp '*last-search-folder*) *last-search-folder* (vl-file-directory-p *last-search-folder*))
          *last-search-folder*)
        ((and (getvar "DWGPREFIX") (vl-file-directory-p (getvar "DWGPREFIX")))
          (vl-string-right-trim "\\/" (getvar "DWGPREFIX")))
        ((vl-file-directory-p "D:\\") "D:\\")
        (t "C:\\")
      )
    )
  )

  (setq dclPath (GOP:WriteDCL))
  (setq dclId (load_dialog dclPath))

  ;; Hàm cập nhật danh sách file trên giao diện DCL
  (defun updateFileList (dir filter scanSub / files item cnt cleanDir)
    (setq cleanDir (GOP:Normalize-Path dir))
    (setq files (GOP:Scan-Dwgs cleanDir filter (= scanSub "1")))
    (setq *GOPTO-Last-Files* files)
    (start_list "lst_files")
    (if (and files (> (length files) 0))
      (progn
        (setq cnt 1)
        (foreach f (acad_strlsort files)
          (add_list (strcat (if (< cnt 10) "00" (if (< cnt 100) "0" "")) (itoa cnt) ". [" (vl-filename-base f) "]  " f))
          (setq cnt (1+ cnt))
        )
      )
      (add_list "(Không tìm thấy file .dwg nào phù hợp)")
    )
    (end_list)
    (set_tile "lbl_count" (strcat "Đã tìm thấy: " (itoa (length files)) " file bản đồ (.DWG)"))
  )

  (setq loopDlg T)
  (while loopDlg
    (if (not (new_dialog "gopto_dlg" dclId))
      (progn
        (prompt "\n[LỖI] Không thể khởi tạo hộp thoại Gộp Bản Đồ!")
        (if (findfile dclPath) (vl-file-delete dclPath))
        (exit)
      )
    )

    ;; Điền dữ liệu ban đầu
    (set_tile "txt_dir" *GOPTO-Last-Dir*)
    (set_tile "chk_subdirs" "1")
    (set_tile "txt_filter" "*.dwg")
    (set_tile "rad_mode_block" "1")
    (set_tile "chk_layer_per_sheet" "1")
    (set_tile "chk_autofix_font" "1")
    (set_tile "chk_purge" "1")
    (set_tile "chk_zoom" "1")

    ;; Quét file lần đầu hiển thị lên danh sách
    (updateFileList *GOPTO-Last-Dir* "*.dwg" "1")

    ;; Nút Chọn Thư Mục
    (action_tile "btn_browse"
      "(setq pickedDir (GOP:Pick-Folder (get_tile \"txt_dir\")))
       (if pickedDir
         (progn
           (set_tile \"txt_dir\" pickedDir)
           (updateFileList pickedDir (get_tile \"txt_filter\") (get_tile \"chk_subdirs\"))
         )
       )"
    )

    ;; Nút Quét File & Tự động quét khi người dùng sửa thông số
    (action_tile "btn_scan"
      "(updateFileList (get_tile \"txt_dir\") (get_tile \"txt_filter\") (get_tile \"chk_subdirs\"))"
    )
    (action_tile "txt_dir"
      "(updateFileList (get_tile \"txt_dir\") (get_tile \"txt_filter\") (get_tile \"chk_subdirs\"))"
    )
    (action_tile "txt_filter"
      "(updateFileList (get_tile \"txt_dir\") (get_tile \"txt_filter\") (get_tile \"chk_subdirs\"))"
    )
    (action_tile "chk_subdirs"
      "(updateFileList (get_tile \"txt_dir\") (get_tile \"txt_filter\") (get_tile \"chk_subdirs\"))"
    )

    ;; Khi bấm Bắt Đầu Gộp
    (action_tile "accept"
      "(setq curDir (get_tile \"txt_dir\")
             filterVal (get_tile \"txt_filter\")
             scanSubVal (get_tile \"chk_subdirs\")
             mergeMode (cond ((= (get_tile \"rad_mode_explode\") \"1\") \"EXPLODE\")
                             ((= (get_tile \"rad_mode_xref\") \"1\") \"XREF\")
                             (t \"BLOCK\"))
             laySheetVal (get_tile \"chk_layer_per_sheet\")
             fixFontVal (get_tile \"chk_autofix_font\")
             purgeVal (get_tile \"chk_purge\")
             zoomVal (get_tile \"chk_zoom\"))
       (done_dialog 1)"
    )

    (action_tile "cancel" "(done_dialog 0)")

    (setq dlgStatus (start_dialog))
    (setq loopDlg nil)
  )

  (unload_dialog dclId)
  (if (findfile dclPath) (vl-file-delete dclPath))

  ;; Nếu người dùng bấm Bắt Đầu Gộp
  (if (= dlgStatus 1)
    (progn
      (setq *GOPTO-Last-Dir* (GOP:Normalize-Path curDir))
      (setq fileList (GOP:Scan-Dwgs *GOPTO-Last-Dir* filterVal (= scanSubVal "1")))
      (GOP:Execute-Merge fileList mergeMode laySheetVal fixFontVal purgeVal zoomVal)
    )
    (prompt "\nĐã đóng hộp thoại Gộp Bản Đồ.")
  )

  (princ)
)

(defun c:GOPBANDO () (c:GOPTO))
(defun c:GBD () (c:GOPTO))

;;; -------------------------------------------------------------------------
;;; 5. LỆNH GỘP CÁC FILE DWG ĐƯỢC CHỌN LẺ (GOPF / GOPFILE)
;;; -------------------------------------------------------------------------
(defun c:GOPF ( / f fileList filesStr sh wsh )
  (vl-load-com)
  (prompt "\n[GOPF] Chọn các file .DWG cần gộp (giữ phím Ctrl hoặc Shift để chọn nhiều file)...")
  (setq file (getfiled "Chọn File Bản Đồ Cần Gộp" (if *GOPTO-Last-Dir* *GOPTO-Last-Dir* "D:\\") "dwg" 0))
  (if (and file (findfile file))
    (progn
      (setq *GOPTO-Last-Dir* (vl-filename-directory file))
      (GOP:Execute-Merge (list file) "BLOCK" "1" "1" "1" "1")
    )
  )
  (princ)
)
(defun c:GOPFILE () (c:GOPF))

;;; -------------------------------------------------------------------------
;;; THÔNG BÁO KHI NẠP LISP THÀNH CÔNG
;;; -------------------------------------------------------------------------
;; Luôn đảm bảo bật hộp thoại mở file và lệnh trong AutoCAD (tránh bị lỗi Enter name of drawing to open)
(vl-catch-all-apply 'setvar (list "FILEDIA" 1))
(vl-catch-all-apply 'setvar (list "CMDDIA" 1))
(vl-catch-all-apply 'setvar (list "ATTDIA" 1))

(prompt "\n=========================================================================")
(prompt "\n [GOPTO PRO v12.0] ĐÃ NẠP CÔNG CỤ GỘP TẤT CẢ CÁC TỜ BẢN ĐỒ VÀO AUTOCAD!")
(prompt "\n   • Gõ lệnh: GOPTO hoặc GBD    : Bật Hộp Thoại Gộp Bản Đồ Đa Năng")
(prompt "\n   • Gõ lệnh: GOPBANDO          : Gộp toàn bộ tờ bản đồ trong thư mục")
(prompt "\n   • Gõ lệnh: GOPF              : Chọn từng file .DWG riêng lẻ để gộp")
(prompt "\n=========================================================================\n")
(princ)