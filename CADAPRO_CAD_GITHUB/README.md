# 🗺️ CadaPro-CAD (Tra Cứu Bản Đồ Địa Chính Siêu Tốc)

> **Bộ công cụ tra cứu số tờ, số thửa, chủ sử dụng, số Giấy chứng nhận (GCN), địa chỉ và tọa độ VN-2000 siêu tốc trên nền tảng AutoCAD.**
> Tương thích toàn diện từ **AutoCAD 2007** đến **AutoCAD 2026+**.

---

## 🌟 Tính Năng Nổi Bật

- ⚡ **Tìm kiếm siêu tốc**: Quét hàng triệu thực thể bản đồ trong vòng vài mili-giây nhờ nhân C# tối ưu hóa bộ nhớ (`FastDwgSearch.cs`).
- 🔤 **Giải mã đa bảng mã thông minh**: Tự động nhận diện và chuyển đổi Unicode, VNI-Windows, TCVN3 (ABC), Vietware... gõ không dấu hoặc có dấu đều tìm chính xác.
- 🎯 **Định vị & Zoom tức thì (VN-2000)**: Chọn kết quả trên danh sách, bản vẽ tự động mở và Zoom chính xác vào tọa độ thửa đất kèm hiệu ứng khoanh vùng Spotlight.
- 📁 **Tra cứu theo thư mục & Đa bản vẽ**: Hỗ trợ quét toàn bộ các tờ bản đồ (.DWG) trong một thư mục chỉ định hoặc trên toàn bộ cơ sở dữ liệu.
- 🛠️ **Sửa lỗi Font tiếng Việt hàng loạt**: Tự động chuẩn hóa Style font cad (SHX và TTF), khắc phục triệt để tình trạng lỗi font hiển thị `?` hoặc ký tự lạ.
- 📦 **Gộp bản đồ địa chính (GOPTO)**: Ghép nối hàng loạt tờ bản đồ vệ tinh / địa chính về chung một tọa độ bản vẽ duy nhất.
- 🔄 **Cập nhật 1-Click từ GitHub**: Bất kỳ máy nào cũng có thể cập nhật bản mới nhất qua lệnh `CAPNHAT` trong AutoCAD hoặc chạy `CAP_NHAT_TU_GITHUB.bat`.

---

## ⌨️ Bảng Lệnh Phím Tắt Trong AutoCAD

| Lệnh | Mô tả chi tiết |
| :--- | :--- |
| `FF` hoặc `TC` | Mở hộp thoại tra cứu địa chính toàn diện |
| `FFF` hoặc `TCTM` | Mở nhanh giao diện Tra cứu Thư mục bản đồ |
| `ZZ` hoặc `DINHVI` | Nhảy và Zoom tới tọa độ của thửa đất vừa chọn |
| `FFN` | Nhảy nhanh tới kết quả tiếp theo (Next) |
| `FFP` | Nhảy lùi về kết quả trước đó (Previous) |
| `FFM` | Khoanh vòng tròn đỏ nổi bật toàn bộ các kết quả tìm thấy |
| `FFC` | Xóa sạch toàn bộ các vòng tròn đánh dấu trên bản vẽ |
| `SF` hoặc `SUAFONT` | Tự động quét và sửa lỗi toàn bộ font chữ trên bản vẽ |
| `KTD` hoặc `KIEMTRA`| Kiểm tra định dạng bảng mã (Unicode/VNI/TCVN3) của Text đang chọn |
| `GBD` hoặc `GOPTO` | Gộp tất cả các tờ bản đồ (.DWG) trong thư mục về bản vẽ hiện tại |
| `CAPNHAT` / `UPDATE`| Tự động kiểm tra và tải bản cập nhật mới nhất từ GitHub |
| `TT` hoặc `TANGTOC` | Tối ưu hóa biến hệ thống AutoCAD giúp thao tác mượt mà nhất |

---

## 🏗️ Cấu Trúc Dự Án

```
📁 CadaPro-CAD/
├── 📄 FastDwgSearch.cs          # Mã nguồn lõi tìm kiếm C# hiệu năng cao
├── ⚙️ FastDwgSearch.exe         # Ứng dụng tra cứu GUI & CLI biên dịch từ C#
├── 📜 TRACUU.LSP               # Module AutoLISP chính tích hợp trong AutoCAD
├── 📜 GOPTO.LSP                # Module gộp bản đồ địa chính
├── 📜 acaddoc.lsp              # Tự động nạp bộ công cụ mỗi khi mở bản vẽ
├── 🚀 CAP_NHAT_TU_GITHUB.bat   # Script 1-click tự động tải bản mới nhất từ GitHub
├── 🚀 compile_cs.bat           # Script 1-click biên dịch lại C# bằng CSC .NET
├── 📦 CaiDat.ps1               # Script PowerShell cài đặt tự động
└── 🚀 CAI_DAT_TOAN_BO_1_CLICK.bat # Trình cài đặt 1-click cho người dùng cuối
```

---

## 🔨 Hướng Dẫn Biên Dịch Mã Nguồn C#

Dự án sử dụng trình biên dịch tích hợp sẵn trong mọi máy tính Windows (`csc.exe` của .NET Framework 4.0/4.8), **không cần cài đặt Visual Studio cồng kềnh**:

Chỉ cần nhấp đúp chạy file **`compile_cs.bat`**, hệ thống sẽ tự động build `FastDwgSearch.exe` và đồng bộ sang tất cả các thư mục hoạt động.

---

## 🔄 Cập Nhật Mã Nguồn Từ GitHub

Khi có bản cập nhật mới trên GitHub:
1. Gõ lệnh **`CAPNHAT`** ngay trong AutoCAD, hoặc nhấp đúp file **`CAP_NHAT_TU_GITHUB.bat`**.
2. Công cụ sẽ tự động tải các file mới nhất từ repository `datbt204-boop/CadaPro-CAD` và đồng bộ vào hệ thống.
