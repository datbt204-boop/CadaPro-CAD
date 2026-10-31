@echo off
chcp 65001 >nul
title CẬP NHẬT TRA CỨU ĐỊA CHÍNH TỪ GITHUB
echo ===================================================================
echo     CÔNG CỤ CẬP NHẬT TRA CỨU BẢN ĐỒ ĐỊA CHÍNH SIÊU TỐC TỪ GITHUB
echo ===================================================================
echo.

set REPO_CONFIG=%~dp0github_repo.cfg
if exist "%REPO_CONFIG%" (
    set /p GITHUB_REPO=<"%REPO_CONFIG%"
)

if "%GITHUB_REPO%"=="" (
    echo [THÔNG BÁO] Chưa cấu hình địa chỉ kho GitHub.
    echo Vui lòng nhập đường dẫn repository của bạn (dạng: username/repo_name):
    set /p GITHUB_REPO="-> Repository: "
    if not "%GITHUB_REPO%"=="" (
        echo %GITHUB_REPO%>"%REPO_CONFIG%"
    )
)

if "%GITHUB_REPO%"=="" (
    echo [LỖI] Bạn chưa cung cấp địa chỉ GitHub!
    pause
    exit /b 1
)

echo.
echo -> Đang kết nối tới GitHub: https://github.com/%GITHUB_REPO%
echo.

set RAW_BASE=https://raw.githubusercontent.com/%GITHUB_REPO%/main
set WORK_DIR=%~dp0

powershell -NoProfile -ExecutionPolicy Bypass -Command ^
    "$repo = '%GITHUB_REPO%';" ^
    "$raw = '%RAW_BASE%';" ^
    "$work = '%WORK_DIR%';" ^
    "$files = @('TRACUU.LSP', 'GOPTO.LSP', 'FastDwgSearch.exe', 'FastDwgSearch.cs', 'compile_cs.bat');" ^
    "foreach ($f in $files) {" ^
    "    $url = \"$raw/$f\";" ^
    "    $dest = Join-Path $work $f;" ^
    "    Write-Host \"-> Đang tải: $f ...\" -ForegroundColor Cyan;" ^
    "    try {" ^
    "        Invoke-WebRequest -Uri $url -OutFile $dest -UseBasicParsing;" ^
    "        Write-Host \"   [OK] Đã cập nhật $f\" -ForegroundColor Green;" ^
    "    } catch {" ^
    "        Write-Host \"   [BỎ QUA] Không tìm thấy hoặc lỗi tải: $f\" -ForegroundColor Yellow;" ^
    "    }" ^
    "};" ^
    "$exeSrc = Join-Path $work 'FastDwgSearch.exe';" ^
    "if (Test-Path $exeSrc) {" ^
    "    foreach ($target in @('C:\EXTRACUUAUTOCAD\FastDwgSearch.exe', 'D:\EXTRACUUAUTOCAD\FastDwgSearch.exe', \"$env:USERPROFILE\Desktop\TraCuuDiaChinh.exe\")) {" ^
    "        $dir = Split-Path $target;" ^
    "        if (Test-Path $dir) { Copy-Item $exeSrc $target -Force; Write-Host \"   [ĐỒNG BỘ] -> $target\" -ForegroundColor Gray; }" ^
    "    }" ^
    "};" ^
    "$lspSrc = Join-Path $work 'TRACUU.LSP';" ^
    "if (Test-Path $lspSrc) {" ^
    "    foreach ($target in @('C:\EXTRACUUAUTOCAD\TRACUU.LSP', 'D:\EXTRACUUAUTOCAD\TRACUU.LSP')) {" ^
    "        $dir = Split-Path $target;" ^
    "        if (Test-Path $dir) { Copy-Item $lspSrc $target -Force; Write-Host \"   [ĐỒNG BỘ] -> $target\" -ForegroundColor Gray; }" ^
    "    }" ^
    "}"

echo.
echo ===================================================================
echo   [HOÀN TẤT] Bản vẽ / AutoCAD đã sẵn sàng sử dụng bản mới nhất!
echo   (Nếu đang mở AutoCAD, bạn chỉ cần gõ APPLOAD nạp lại TRACUU.LSP)
echo ===================================================================
echo.
pause
