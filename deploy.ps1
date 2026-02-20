$ErrorActionPreference = "Stop"
$SCRIPT_DIR = Split-Path -Parent $MyInvocation.MyCommand.Path

function New-Symlink {
    param(
        [string]$Filename,
        [string]$SubDir
    )

    $source = Join-Path $SCRIPT_DIR $Filename

    $destDir = switch ($SubDir) {
        "APPDATA"      { $env:APPDATA }
        "LOCALAPPDATA" { $env:LOCALAPPDATA }
        "USERPROFILE"  { $env:USERPROFILE }
        ""             { $env:USERPROFILE }
        default        { Join-Path $env:USERPROFILE $SubDir }
    }

    $destination = Join-Path $destDir $Filename

    $parentDir = Split-Path -Parent $destination
    if (-not (Test-Path $parentDir)) {
        New-Item -ItemType Directory -Path $parentDir -Force | Out-Null
    }

    $existing = Get-Item $destination -ErrorAction SilentlyContinue
    if ($existing -and $existing.LinkType -eq "SymbolicLink") {
        Write-Host "[WARNING] $Filename already symlinked" -ForegroundColor Yellow
        return
    }

    if (Test-Path $destination) {
        Write-Host "[ERROR] $destination exists but it's not a symlink. Please fix that manually" -ForegroundColor Red
        exit 1
    }

    $itemType = if (Test-Path $source -PathType Container) { "Junction" } else { "SymbolicLink" }

    New-Item -ItemType $itemType -Path $destination -Target $source | Out-Null
    Write-Host "[OK] $source -> $destination" -ForegroundColor Green
}

function Deploy-Manifest {
    param([string]$ManifestFile)

    $manifestPath = Join-Path $SCRIPT_DIR $ManifestFile

    foreach ($row in Get-Content $manifestPath) {
        if ($row -match '^\s*#' -or [string]::IsNullOrWhiteSpace($row)) {
            continue
        }

        $parts = $row -split '\|'
        $filename  = $parts[0].Trim()
        $operation = $parts[1].Trim()
        $subdest   = if ($parts.Count -ge 3) { $parts[2].Trim() } else { "" }

        switch ($operation) {
            "symlink" {
                New-Symlink -Filename $filename -SubDir $subdest
            }
            default {
                Write-Host "[WARNING] Unknown operation '$operation'. Skipping..." -ForegroundColor Yellow
            }
        }
    }
}

if ($args.Count -eq 0) {
    Write-Host "Usage: $($MyInvocation.MyCommand.Name) <MANIFEST>"
    Write-Host "ERROR: no MANIFEST file provided" -ForegroundColor Red
    exit 1
}

Deploy-Manifest $args[0]
