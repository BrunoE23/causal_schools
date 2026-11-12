# Official classroom-level enrollment aggregates; no student-level data.
$ErrorActionPreference = 'Stop'
$enrollTaskRoot = Join-Path (Get-Location) 'data/clean/secondary_sae_spots/enrollment_trends'
$enrollArchiveRoot = Join-Path (Get-Location) 'tmp/secondary_sae_enrollment_downloads'
$enrollSevenZip = 'C:/Program Files/7-Zip/7z.exe'
New-Item -ItemType Directory -Force -Path $enrollArchiveRoot | Out-Null
New-Item -ItemType Directory -Force -Path $enrollTaskRoot | Out-Null
$enrollManifest = @()
foreach ($enrollYear in 2010..2025) {
    $enrollUrl = if ($enrollYear -le 2021) {
        'https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/Resumen-de-matricula-por-curso-' + $enrollYear + '.rar'
    } elseif ($enrollYear -eq 2022) {
        'https://datosabiertos.mineduc.cl/wp-content/uploads/2022/10/Resumen-de-Matricula-por-curso-2022.rar'
    } elseif ($enrollYear -eq 2023) {
        'https://datosabiertos.mineduc.cl/wp-content/uploads/2023/10/Resumen-de-Matricula-por-curso-2023.rar'
    } else {
        'https://datosabiertos.mineduc.cl/wp-content/uploads/' + $enrollYear + '/11/Resumen-de-matricula-por-curso-' + $enrollYear + '.rar'
    }
    $enrollArchive = Join-Path $enrollArchiveRoot ('courses_' + $enrollYear + '.rar')
    if (-not (Test-Path -LiteralPath $enrollArchive)) {
        $enrollHead = Invoke-WebRequest -Method Head -Uri $enrollUrl -TimeoutSec 30
        $enrollLength = [long]($enrollHead.Headers['Content-Length'] | Select-Object -First 1)
        if ($enrollLength -gt 100000000) { throw ('Unexpectedly large aggregate archive: ' + $enrollYear) }
        Write-Output ('Downloading ' + $enrollYear + ': ' + $enrollLength + ' bytes')
        Invoke-WebRequest -Uri $enrollUrl -OutFile $enrollArchive -TimeoutSec 120
        if ($enrollLength -gt 0 -and (Get-Item -LiteralPath $enrollArchive).Length -ne $enrollLength) { throw 'Incomplete archive' }
    }
    $enrollTarget = Join-Path $enrollTaskRoot ('source_courses/' + $enrollYear)
    & $enrollSevenZip x $enrollArchive ('-o' + $enrollTarget) '-ir!*.csv' '-ir!*.pdf' -aos | Select-Object -Last 4
    if ($LASTEXITCODE -ne 0) { throw ('Extraction failed: ' + $enrollYear) }
    $enrollManifest += [PSCustomObject]@{year=$enrollYear;url=$enrollUrl;archive=$enrollArchive;bytes=(Get-Item -LiteralPath $enrollArchive).Length;sha256=(Get-FileHash -LiteralPath $enrollArchive -Algorithm SHA256).Hash}
}
$enrollManifest | Export-Csv -Path (Join-Path $enrollTaskRoot 'download_manifest.csv') -NoTypeInformation
