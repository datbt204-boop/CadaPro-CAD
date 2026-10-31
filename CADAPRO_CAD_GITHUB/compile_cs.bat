@echo off
chcp 65001 >nul
echo ========================================================
echo   BIÊN DỊCH FastDwgSearch.exe TỪ MÃ NGUỒN C#
echo ========================================================
set CSC_EXE=C:\Windows\Microsoft.NET\Framework64\v4.0.30319\csc.exe
if not exist "%CSC_EXE%" (
    set CSC_EXE=C:\Windows\Microsoft.NET\Framework\v4.0.30319\csc.exe
)
if not exist "%CSC_EXE%" (
    echo [LỖI] Không tìm thấy trình biên dịch csc.exe của .NET Framework 4.0/4.8!
    pause
    exit /b 1
)

echo Đang biên dịch FastDwgSearch.cs...
"%CSC_EXE%" /target:winexe /unsafe /optimize /out:FastDwgSearch.exe FastDwgSearch.cs /win32icon:app_icon.ico
if %ERRORLEVEL% equ 0 (
    echo [THÀNH CÔNG] Đã tạo FastDwgSearch.exe thành công!
    copy /y FastDwgSearch.exe TraCuuDiaChinh.exe >nul 2>&1
    if exist "D:\EXTRACUUAUTOCAD\" copy /y FastDwgSearch.exe "D:\EXTRACUUAUTOCAD\" >nul 2>&1
    if exist "C:\EXTRACUUAUTOCAD\" copy /y FastDwgSearch.exe "C:\EXTRACUUAUTOCAD\" >nul 2>&1
    if exist "%USERPROFILE%\Desktop\TraCuuDiaChinh.exe" copy /y FastDwgSearch.exe "%USERPROFILE%\Desktop\TraCuuDiaChinh.exe" >nul 2>&1
    echo Đã đồng bộ sang các thư mục hoạt động!
) else (
    echo [THẤT BẠI] Có lỗi trong quá trình biên dịch!
)
pause
