param(
    [Parameter(Mandatory=$true)]
    [string]$NewPath,

    [switch]$Remove  # optional: env "C:\some\path" -Remove
)

$current = [Environment]::GetEnvironmentVariable("PATH", "User")
$entries = $current -split ";" | Where-Object { $_ -ne "" }

if ($Remove) {
    $updated = ($entries | Where-Object { $_ -ne $NewPath.TrimEnd("\") -and $_ -ne $NewPath }) -join ";"
    [Environment]::SetEnvironmentVariable("PATH", $updated, "User")
    Write-Host "Removed: $NewPath" -ForegroundColor Yellow
} else {
    $normalized = $NewPath.TrimEnd("\")
    $alreadyExists = $entries | Where-Object { $_.TrimEnd("\") -eq $normalized }

    if ($alreadyExists) {
        Write-Host "Already in PATH: $NewPath" -ForegroundColor Cyan
    } else {
        $updated = ($entries + $NewPath) -join ";"
        [Environment]::SetEnvironmentVariable("PATH", $updated, "User")
        Write-Host "Added: $NewPath" -ForegroundColor Green
    }
}

Write-Host "Restart your terminal for changes to take effect."
