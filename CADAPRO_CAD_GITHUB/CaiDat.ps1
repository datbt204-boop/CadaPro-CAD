[Console]::OutputEncoding = [System.Text.Encoding]::UTF8
[Console]::Title = "CAI DAT BO FONT CAD VA CONG CU TRA CUU (PRO v12.0 - WINDOWS 10)"

function Test-Admin {
    $user = [Security.Principal.WindowsIdentity]::GetCurrent()
    (New-Object Security.Principal.WindowsPrincipal($user)).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

if (-not (Test-Admin)) {
    try {
        Write-Host "[THONG BAO] Dang yeu cau quyen Administrator de cai dat he thong..." -ForegroundColor Yellow
        Start-Process powershell.exe -ArgumentList "-NoProfile -ExecutionPolicy Bypass -File `"$PSCommandPath`"" -Verb RunAs -ErrorAction Stop
        exit
    } catch {
        Write-Host "[CANH BAO] Tiep tuc cai dat o che do nguoi dung hien tai..." -ForegroundColor Yellow
    }
}

# --- XAC DINH THU MUC BO CAI DAT (DU DIEU KIEN CHAY O MOI VI TRI) ---
$ScriptDir = Split-Path -Parent $PSCommandPath
$ParentDir = Split-Path -Parent $ScriptDir

$PackageRoot = $ScriptDir
if (-not (Test-Path (Join-Path $PackageRoot "1_Fonts_CAD_SHX")) -and (Test-Path (Join-Path $ParentDir "1_Fonts_CAD_SHX"))) {
    $PackageRoot = $ParentDir
}
Set-Location $PackageRoot

# Ham ho tro tim file trong bo cai
function Get-PackageFilePath ($fileName) {
    $cands = @(
        (Join-Path $PackageRoot $fileName),
        (Join-Path $ScriptDir $fileName),
        (Join-Path $PackageRoot (Join-Path "_HeThong" $fileName)),
        (Join-Path $ScriptDir (Join-Path "_HeThong" $fileName)),
        (Join-Path "D:\EXTRACUUAUTOCAD" $fileName),
        (Join-Path "C:\EXTRACUUAUTOCAD" $fileName)
    )
    foreach ($c in $cands) {
        if (-not [string]::IsNullOrEmpty($c) -and (Test-Path $c)) { return $c }
    }
    return $null
}

Write-Host "=========================================================================" -ForegroundColor Cyan
Write-Host "   BO CAI DAT BO FONT CAD TOAN NANG VA TRA CUU DIA CHINH (PRO v12.0)" -ForegroundColor Green
Write-Host "                 PHIEN BAN HOAN THIEN CHO WINDOWS 10" -ForegroundColor White
Write-Host "=========================================================================" -ForegroundColor Cyan
Write-Host " Thu muc goc: $PackageRoot" -ForegroundColor Gray
Write-Host ""

# =====================================================================
# 1. Cai Font Windows TTF/OTF (TCVN3, VNI, Unicode) + Kich hoat tuc thi
# =====================================================================
Write-Host "[1/4] Dang cai dat toan bo Font Windows (TCVN3, VNI, Unicode)..." -ForegroundColor Yellow

# Nap Win32 API de dang ky va thong bao Font cho Windows 10 tuc thi ma khong can khoi dong lai
try {
    Add-Type -TypeDefinition @"
    using System;
    using System.Runtime.InteropServices;
    public class WinFontManager {
        [DllImport("gdi32.dll", EntryPoint="AddFontResourceW", SetLastError=true, CharSet=CharSet.Unicode)]
        public static extern int AddFontResource(string lpFileName);

        [DllImport("user32.dll", SetLastError=true, CharSet=CharSet.Auto)]
        public static extern int SendMessage(IntPtr hWnd, uint Msg, IntPtr wParam, IntPtr lParam);
    }
"@ -ErrorAction SilentlyContinue
} catch {}

$ttfFolder = Join-Path $PackageRoot "2_Fonts_Windows_TTF"
if (-not (Test-Path $ttfFolder)) {
    $candTtf = Get-ChildItem -Path $PackageRoot -Directory -Filter "*TTF*" -Recurse -ErrorAction SilentlyContinue | Select-Object -First 1
    if ($candTtf) { $ttfFolder = $candTtf.FullName }
}

$winFonts = "$env:SystemRoot\Fonts"
$fontRegKeyLM = "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Fonts"
$fontRegKeyCU = "HKCU:\Software\Microsoft\Windows NT\CurrentVersion\Fonts"

if (Test-Path $ttfFolder) {
    $ttfFiles = Get-ChildItem -Path $ttfFolder -Include *.ttf, *.otf -Recurse -ErrorAction SilentlyContinue
    $installedTtf = 0
    $totalTtf = $ttfFiles.Count
    $step = [Math]::Max(1, [int]($totalTtf / 10))
    $idx = 0

    foreach ($file in $ttfFiles) {
        $idx++
        $dest = Join-Path $winFonts $file.Name
        $needCopy = $true
        if (Test-Path $dest) {
            if ((Get-Item $dest).Length -eq $file.Length) {
                $needCopy = $false
            }
        }
        
        if ($needCopy) {
            try {
                Copy-Item -Path $file.FullName -Destination $dest -Force -ErrorAction SilentlyContinue
                $installedTtf++
            } catch {}
        }
        
        $fontName = [System.IO.Path]::GetFileNameWithoutExtension($file.Name) + " (TrueType)"
        Set-ItemProperty -Path $fontRegKeyLM -Name $fontName -Value $file.Name -ErrorAction SilentlyContinue
        Set-ItemProperty -Path $fontRegKeyCU -Name $fontName -Value $file.Name -ErrorAction SilentlyContinue

        try {
            [WinFontManager]::AddFontResource($dest) | Out-Null
        } catch {}

        if ($idx % $step -eq 0 -or $idx -eq $totalTtf) {
            $pct = [int](($idx / $totalTtf) * 100)
            Write-Host -NoNewline "`r   -> Tien trinh: $idx/$totalTtf fonts ($pct%)...                    "
        }
    }
    Write-Host ""

    # Thong bao toan he thong Windows 10 lam moi bang ma Font (WM_FONTCHANGE)
    try {
        [WinFontManager]::SendMessage([IntPtr]0xffff, 0x001d, [IntPtr]::Zero, [IntPtr]::Zero) | Out-Null
    } catch {}

    Write-Host "   -> Da dong bo $totalTtf Font Windows va dang ky thanh cong vao Windows 10!" -ForegroundColor Green
} else {
    Write-Host "   -> [BO QUA] Khong tim thay thu muc 2_Fonts_Windows_TTF." -ForegroundColor Gray
}

# =====================================================================
# 2. Cai Font CAD SHX/RSC vao moi phien ban AutoCAD
# =====================================================================
Write-Host ""
Write-Host "[2/4] Dang cai dat toan bo Font CAD (.SHX / .RSC) vao AutoCAD..." -ForegroundColor Yellow
$shxFolder = Join-Path $PackageRoot "1_Fonts_CAD_SHX"
if (-not (Test-Path $shxFolder)) {
    $candShx = Get-ChildItem -Path $PackageRoot -Directory -Filter "*SHX*" -Recurse -ErrorAction SilentlyContinue | Select-Object -First 1
    if ($candShx) { $shxFolder = $candShx.FullName }
}

$acadFontDirs = [System.Collections.Generic.HashSet[string]]::new([System.StringComparer]::OrdinalIgnoreCase)

# Quet thu muc cai dat AutoCAD trong Program Files (x64 va x86)
$searchBases = @("${env:ProgramFiles}\Autodesk", "${env:ProgramFiles(x86)}\Autodesk")
foreach ($sb in $searchBases) {
    if (Test-Path $sb) {
        Get-ChildItem -Path $sb -Directory -Filter "AutoCAD*" -ErrorAction SilentlyContinue | ForEach-Object {
            $fDir = Join-Path $_.FullName "Fonts"
            if (Test-Path $fDir) { [void]$acadFontDirs.Add($fDir) }
            $sDir = Join-Path $_.FullName "Support"
            if (Test-Path $sDir) { [void]$acadFontDirs.Add($sDir) }
        }
    }
}

# Quet duong dan AutoCAD tu Registry
$regBases = @("HKLM:\SOFTWARE\Autodesk\AutoCAD", "HKCU:\SOFTWARE\Autodesk\AutoCAD")
foreach ($rb in $regBases) {
    if (Test-Path $rb) {
        Get-ChildItem -Path $rb -Recurse -ErrorAction SilentlyContinue | ForEach-Object {
            $loc = (Get-ItemProperty -Path $_.PSPath -Name "Location" -ErrorAction SilentlyContinue).Location
            if ($loc -and (Test-Path $loc)) {
                $fDir = Join-Path $loc "Fonts"
                if (Test-Path $fDir) { [void]$acadFontDirs.Add($fDir) }
                $sDir = Join-Path $loc "Support"
                if (Test-Path $sDir) { [void]$acadFontDirs.Add($sDir) }
            }
        }
    }
}

# Quet thu muc Support & Fonts trong Roaming / Local AppData
Get-ChildItem -Path "$env:APPDATA\Autodesk", "$env:LOCALAPPDATA\Autodesk" -Directory -Recurse -Depth 5 -ErrorAction SilentlyContinue | Where-Object { $_.Name -ieq "Fonts" -or $_.Name -ieq "Support" } | ForEach-Object {
    [void]$acadFontDirs.Add($_.FullName)
}

# Them thu muc font tap trung he thong
if (Test-Path "D:\") { [void]$acadFontDirs.Add("D:\EXTRACUUAUTOCAD\Fonts") }
[void]$acadFontDirs.Add("C:\EXTRACUUAUTOCAD\Fonts")

if (Test-Path $shxFolder) {
    $shxFiles = Get-ChildItem -Path $shxFolder -Include *.shx, *.rsc, *.fnt -Recurse -ErrorAction SilentlyContinue
    $copiedShx = 0
    foreach ($aDir in $acadFontDirs) {
        if (-not (Test-Path $aDir)) {
            try { New-Item -ItemType Directory -Path $aDir -Force | Out-Null } catch {}
        }
        if (Test-Path $aDir) {
            foreach ($file in $shxFiles) {
                $dest = Join-Path $aDir $file.Name
                $needCopy = $true
                if (Test-Path $dest) {
                    if ((Get-Item $dest).Length -eq $file.Length) { $needCopy = $false }
                }
                if ($needCopy) {
                    try {
                        Copy-Item -Path $file.FullName -Destination $dest -Force -ErrorAction SilentlyContinue
                        $copiedShx++
                    } catch {}
                }
            }
        }
    }
    Write-Host "   -> Da dong bo $($shxFiles.Count) Font CAD net don vao $($acadFontDirs.Count) thu muc AutoCAD!" -ForegroundColor Green
} else {
    Write-Host "   -> [BO QUA] Khong tim thay thu muc 1_Fonts_CAD_SHX." -ForegroundColor Gray
}

# =====================================================================
# 3. Cai Dat Bo May Tra Cuu & Co So Du Lieu Ban Do
# =====================================================================
Write-Host ""
Write-Host "[3/4] Dang cai dat Bo may Tra cuu Sieu toc va Database ban do..." -ForegroundColor Yellow

$coreDirs = [System.Collections.Generic.List[string]]::new()
if (Test-Path "D:\") {
    $coreDirs.Add("D:\EXTRACUUAUTOCAD")
    $coreDirs.Add("D:\VBA")
}
$coreDirs.Add("C:\EXTRACUUAUTOCAD")
$coreDirs.Add("C:\VBA")

foreach ($cd in $coreDirs) {
    if (-not (Test-Path $cd)) {
        try { New-Item -ItemType Directory -Path $cd -Force | Out-Null } catch {}
    }
}

$coreFiles = @(
    "TRACUU.LSP",
    "FastDwgSearch.exe",
    "FastDwgSearch.cs",
    "FOLDER_INDEX_DB.tsv",
    "FOLDER_INDEX_DB.dwgmap",
    "FOLDER_MANIFEST.tsv",
    "acaddoc.lsp"
)

foreach ($cd in $coreDirs) {
    if (Test-Path $cd) {
        foreach ($cf in $coreFiles) {
            $src = Get-PackageFilePath $cf
            if ($src -and (Test-Path $src)) {
                $dst = Join-Path $cd $cf
                $needCopy = $true
                if (Test-Path $dst) {
                    if ((Get-Item $dst).Length -eq (Get-Item $src).Length) { $needCopy = $false }
                }
                if ($needCopy) {
                    try { Copy-Item -Path $src -Destination $dst -Force -ErrorAction SilentlyContinue } catch {}
                }
            }
        }
    }
}
Write-Host "   -> Da cai dat day du bo may tra cuu vao: $($coreDirs -join ', ')!" -ForegroundColor Green

# =====================================================================
# 4. Cau Hinh Tu Dong Nap acaddoc.lsp Khi Mo AutoCAD
# =====================================================================
Write-Host ""
Write-Host "[4/4] Dang cau hinh tu dong nap cong cu khi khoi dong AutoCAD..." -ForegroundColor Yellow

$acadSupportDirs = [System.Collections.Generic.HashSet[string]]::new([System.StringComparer]::OrdinalIgnoreCase)

Get-ChildItem -Path "$env:APPDATA\Autodesk", "$env:LOCALAPPDATA\Autodesk" -Directory -Recurse -Depth 5 -ErrorAction SilentlyContinue | Where-Object { $_.Name -ieq "Support" } | ForEach-Object {
    [void]$acadSupportDirs.Add($_.FullName)
}

$lines = @(
    "",
    ";;; --- TOI UU MO FILE & TRA CUU DIA CHINH SIEU TOC (TURBO PRO v13.5) ---",
    "(vl-load-com)",
    "(vl-catch-all-apply 'setvar (list `"VTENABLE`" 0))",
    "(vl-catch-all-apply 'setvar (list `"WHIPTHREAD`" 3))",
    "(vl-catch-all-apply 'setvar (list `"SELECTIONPREVIEW`" 0))",
    "(vl-catch-all-apply 'setvar (list `"ROLLOVERTOOLTIPS`" 0))",
    "(vl-catch-all-apply 'setvar (list `"PROPERTYPREVIEW`" 0))",
    "(vl-catch-all-apply 'setvar (list `"HPQUICKPREVIEW`" 0))",
    "(vl-catch-all-apply 'setvar (list `"DRAWORDERCTL`" 0))",
    "(vl-catch-all-apply 'setvar (list `"NAVVCUBEDISPLAY`" 0))",
    "(vl-catch-all-apply 'setvar (list `"CACHEMAXFILES`" 2048))",
    "(vl-catch-all-apply 'setvar (list `"CACHEMAXTOTALSIZE`" 4096))",
    "(vl-catch-all-apply 'setvar (list `"DWGCHECK`" 0))",
    "(if (not (fboundp 'c:FF))",
    "  (vl-catch-all-apply",
    "    '(lambda ()",
    "       (cond",
    "         ((findfile `"TRACUU.LSP`")                     (load `"TRACUU.LSP`"))",
    "         ((findfile `"D:/EXTRACUUAUTOCAD/TRACUU.LSP`")  (load `"D:/EXTRACUUAUTOCAD/TRACUU.LSP`"))",
    "         ((findfile `"D:/BO_FONT_CAD_FULL/TRACUU.LSP`") (load `"D:/BO_FONT_CAD_FULL/TRACUU.LSP`"))",
    "         ((findfile `"C:/EXTRACUUAUTOCAD/TRACUU.LSP`")  (load `"C:/EXTRACUUAUTOCAD/TRACUU.LSP`"))",
    "         ((findfile `"C:/BO_FONT_CAD_FULL/TRACUU.LSP`") (load `"C:/BO_FONT_CAD_FULL/TRACUU.LSP`"))",
    "         ((findfile `"D:/VBA/TRACUU.LSP`")              (load `"D:/VBA/TRACUU.LSP`"))",
    "         ((findfile `"C:/VBA/TRACUU.LSP`")              (load `"C:/VBA/TRACUU.LSP`"))",
    "       )",
    "     )",
    "  )",
    ")",
    "(princ)"
)
$loadSnippet = $lines -join "`r`n"

foreach ($sDir in $acadSupportDirs) {
    # Sao chep cac file cot loi truc tiep vao Support cua AutoCAD
    foreach ($cf in @("TRACUU.LSP", "FastDwgSearch.exe", "FastDwgSearch.cs", "FOLDER_INDEX_DB.dwgmap")) {
        $src = Get-PackageFilePath $cf
        if ($src -and (Test-Path $src)) {
            try { Copy-Item -Path $src -Destination (Join-Path $sDir $cf) -Force -ErrorAction SilentlyContinue } catch {}
        }
    }

    # Ghi noi dung vao acaddoc.lsp
    $acadDocPath = Join-Path $sDir "acaddoc.lsp"
    if (Test-Path $acadDocPath) {
        $existing = Get-Content $acadDocPath -Raw -ErrorAction SilentlyContinue
        if ($existing -notmatch "TRACUU\.LSP") {
            [System.IO.File]::AppendAllText($acadDocPath, "`r`n$loadSnippet", [System.Text.Encoding]::UTF8)
        }
    } else {
        [System.IO.File]::WriteAllText($acadDocPath, $loadSnippet.TrimStart(), [System.Text.Encoding]::UTF8)
    }
}
Write-Host "   -> Da tich hop tu dong nap vao $($acadSupportDirs.Count) thu muc AutoCAD Support tren he thong!" -ForegroundColor Green

Write-Host ""
Write-Host "=========================================================================" -ForegroundColor Cyan
Write-Host "   [THANH CONG] DA CAI DAT HOAN TAT 100% CHO WINDOWS 10!" -ForegroundColor Green
Write-Host "=========================================================================" -ForegroundColor Cyan
Write-Host "  Cac lenh san sang su dung ngay trong AutoCAD:" -ForegroundColor White
Write-Host "    - Lenh FF   : Tra cuu dia chinh sieu toc (So thua, To, Chu dat...)" -ForegroundColor Yellow
Write-Host "    - Lenh DF   : Bang doi Font toan nang sang Arial Unicode / TCVN3" -ForegroundColor Yellow
Write-Host "    - Lenh SF   : Tu dong sua loi font tren ban ve trong 1 giay" -ForegroundColor Yellow
Write-Host "    - Lenh TT   : Tang toc AutoCAD cuc dai, muot chuot 100%" -ForegroundColor Yellow
Write-Host "    - Lenh CTTM : Bat/Tat che do tim kiem xuyen toan bo thu muc (.DWG)" -ForegroundColor Yellow
Write-Host "    - Lenh GOPBANDO : Gop nhanh cac to ban do trong thu muc ve 1 file" -ForegroundColor Yellow
Write-Host "=========================================================================" -ForegroundColor Cyan
Write-Host ""
