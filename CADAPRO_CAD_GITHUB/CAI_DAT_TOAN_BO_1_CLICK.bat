@echo off
chcp 65001 >nul
title CAI DAT BO FONT CAD VA CONG CU TRA CUU (PRO v12.0 - WINDOWS 10)

REM Kiem tra quyen Administrator
net session >nul 2>&1
if %errorlevel% neq 0 (
    echo [THONG BAO] Dang yeu cau quyen Administrator de cai dat font he thong va cong cu...
    powershell -NoProfile -ExecutionPolicy Bypass -Command "Start-Process -FilePath '%~f0' -Verb RunAs"
    exit /b
)

cd /d "%~dp0"
if exist "%~dp0CaiDat.ps1" (
    powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0CaiDat.ps1"
) else if exist "%~dp0_HeThong\CaiDat.ps1" (
    powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0_HeThong\CaiDat.ps1"
) else (
    echo [LOI] Khong tim thay file CaiDat.ps1 trong thu muc!
    echo Vui long giai nen day du thu muc truoc khi chay.
    pause
    exit /b 1
)
echo.
pause
