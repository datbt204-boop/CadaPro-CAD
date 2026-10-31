;;; =========================================================================
;;; TU DONG NAP BO CONG CU TRA CUU DIA CHINH, SUA FONT & GOP BAN DO (TURBO PRO v13.5)
;;; =========================================================================
(vl-load-com)

;; Luon luon bat hop thoai mo file va hop thoai lenh (chong loi mat bang chon file)
(vl-catch-all-apply 'setvar (list "FILEDIA" 1))
(vl-catch-all-apply 'setvar (list "CMDDIA" 1))
(vl-catch-all-apply 'setvar (list "ATTDIA" 1))

;;; =========================================================================
;;; TOI UU HOA TANG TOC MO FILE & TRIET TIEU HOAN TOAN LAG CHUOT
;;; =========================================================================
(vl-catch-all-apply 'setvar (list "VTENABLE" 0))          ;; Tat delay hieu ung chuyen canh zoom/pan/mo file
(vl-catch-all-apply 'setvar (list "WHIPTHREAD" 3))        ;; Bat da nhan CPU xu ly do hoa & redraw
(vl-catch-all-apply 'setvar (list "SELECTIONPREVIEW" 0))  ;; Tat preview hover giam giat lag chuot tren ban do day dac
(vl-catch-all-apply 'setvar (list "ROLLOVERTOOLTIPS" 0))  ;; Tat tooltip hoi cham khi luot chuot
(vl-catch-all-apply 'setvar (list "PROPERTYPREVIEW" 0))   ;; Tat tinh toan preview thuoc tinh gay dung hinh
(vl-catch-all-apply 'setvar (list "HPQUICKPREVIEW" 0))    ;; Tat preview Hatch tranh treo khi chuot qua ranh dat
(vl-catch-all-apply 'setvar (list "DRAWORDERCTL" 0))      ;; Toi uu ve do hoa 2D
(vl-catch-all-apply 'setvar (list "NAVVCUBEDISPLAY" 0))   ;; Tat ViewCube 3D tieu ton GPU tren 2D
(vl-catch-all-apply 'setvar (list "CACHEMAXFILES" 2048))  ;; Tang bo dem cache ban ve
(vl-catch-all-apply 'setvar (list "CACHEMAXTOTALSIZE" 4096)) ;; Tang bo dem 4GB RAM cho CAD
(vl-catch-all-apply 'setvar (list "DWGCHECK" 0))          ;; Tat kiem tra CRC gay tre khi doc file

;; Nap TRACUU.LSP (chi nap khi chua co trong session de mo file tuc thi)
(if (not (fboundp 'c:FF))
  (vl-catch-all-apply
    '(lambda ()
       (cond
         ((findfile "TRACUU.LSP")                     (load "TRACUU.LSP"))
         ((findfile "D:/EXTRACUUAUTOCAD/TRACUU.LSP")  (load "D:/EXTRACUUAUTOCAD/TRACUU.LSP"))
         ((findfile "D:/BO_FONT_CAD_FULL/TRACUU.LSP") (load "D:/BO_FONT_CAD_FULL/TRACUU.LSP"))
         ((findfile "C:/EXTRACUUAUTOCAD/TRACUU.LSP")  (load "C:/EXTRACUUAUTOCAD/TRACUU.LSP"))
         ((findfile "C:/BO_FONT_CAD_FULL/TRACUU.LSP") (load "C:/BO_FONT_CAD_FULL/TRACUU.LSP"))
         ((findfile "D:/VBA/TRACUU.LSP")              (load "D:/VBA/TRACUU.LSP"))
         ((findfile "C:/VBA/TRACUU.LSP")              (load "C:/VBA/TRACUU.LSP"))
       )
     )
  )
)

;; Nap GOPTO.LSP (chi nap khi chua co trong session)
(if (not (fboundp 'c:GOPTO))
  (vl-catch-all-apply
    '(lambda ()
       (cond
         ((findfile "GOPTO.LSP")                     (load "GOPTO.LSP"))
         ((findfile "D:/EXTRACUUAUTOCAD/GOPTO.LSP")  (load "D:/EXTRACUUAUTOCAD/GOPTO.LSP"))
         ((findfile "D:/BO_FONT_CAD_FULL/GOPTO.LSP") (load "D:/BO_FONT_CAD_FULL/GOPTO.LSP"))
         ((findfile "C:/EXTRACUUAUTOCAD/GOPTO.LSP")  (load "C:/EXTRACUUAUTOCAD/GOPTO.LSP"))
         ((findfile "C:/BO_FONT_CAD_FULL/GOPTO.LSP") (load "C:/BO_FONT_CAD_FULL/GOPTO.LSP"))
         ((findfile "D:/VBA/GOPTO.LSP")              (load "D:/VBA/GOPTO.LSP"))
         ((findfile "C:/VBA/GOPTO.LSP")              (load "C:/VBA/GOPTO.LSP"))
       )
     )
  )
)

;;; =========================================================================
;;; DINH VI TOA DO KHI MO BAN VE TU APP DESKTOP (FAST FIND PRO v13.5)
;;; =========================================================================
(vl-catch-all-apply
  '(lambda ( / jumpPaths fPath fn line1 line2 line3 remStr pIdx pIdx2 pIdx3 xVal yVal pt targetDwg curDwg)
     (setq jumpPaths (list "D:/EXTRACUUAUTOCAD/fast_jump.tsv"
                           "C:/EXTRACUUAUTOCAD/fast_jump.tsv"
                           "D:/VBA/fast_jump.tsv"
                           "C:/VBA/fast_jump.tsv"
                           "D:/EXTRACUUAUTOCAD/pending_zoom.txt"
                           "C:/EXTRACUUAUTOCAD/pending_zoom.txt"
                           "D:/VBA/pending_zoom.txt"
                           "C:/VBA/pending_zoom.txt"))
     (setq fPath nil xVal nil yVal nil targetDwg "")
     (foreach p jumpPaths
       (if (and (null fPath) (findfile p)) (setq fPath (findfile p)))
     )
     (if (and fPath (setq fn (open fPath "r")))
       (progn
         (setq line1 (read-line fn))
         (if (wcmatch (strcase fPath) "*.TSV")
           (progn
             (close fn)
             (setq pIdx (vl-string-search (chr 9) line1))
             (if pIdx
               (progn
                 (setq targetDwg (substr line1 1 pIdx)
                       remStr    (substr line1 (+ pIdx 2))
                       pIdx2     (vl-string-search (chr 9) remStr))
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
             (setq targetDwg line1
                   line2 (read-line fn)
                   line3 (read-line fn))
             (close fn)
             (if (and line2 line3)
               (setq xVal (distof line2) yVal (distof line3))
             )
           )
         )
         ;; Xoa tat ca cac file pending de khong bao gio bi nhay lap lai
         (foreach p jumpPaths
           (if (findfile p) (vl-catch-all-apply 'vl-file-delete (list (findfile p))))
         )
         ;; Chi thuc hien zoom neu dung ban ve muc tieu
         (setq curDwg (strcase (getvar "DWGNAME")))
         (if (or (null targetDwg)
                 (= targetDwg "")
                 (wcmatch curDwg (strcase (strcat "*" (vl-filename-base targetDwg) "*"))))
           (if (and xVal yVal (/= xVal 0.0) (/= yVal 0.0))
             (progn
               (setq pt (list xVal yVal 0.0))
               (vla-ZoomCenter (vlax-get-acad-object) (vlax-3d-point pt) 45.0)
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
               (princ (strcat "\n>>> [FAST FIND PRO] DA PHONG TOI TOA DO VN-2000: X=" (rtos xVal 2 2) ", Y=" (rtos yVal 2 2) " <<<\n"))
             )
           )
         )
       )
     )
   )
)

(if (not (fboundp 'c:ZZ))
  (defun c:ZZ ()
    (if (fboundp 'TC-Check-Pending-Zoom) (TC-Check-Pending-Zoom) (princ "\nChua co toa do cho."))
    (princ)
  )
)
(princ)