# Retrieve public releases missing from the project's existing 2016-2021 data.
# Existing raw data are never modified. Extract only school offers/documentation.
$ErrorActionPreference = 'Stop'
$saeProjectRoot = (Get-Location).Path
$saeArchiveRoot = Join-Path $saeProjectRoot 'tmp/secondary_sae_downloads'
$saeSourceRoot = Join-Path $saeProjectRoot 'data/clean/secondary_sae_spots/source_offers'
$saeSevenZip = 'C:/Program Files/7-Zip/7z.exe'
if (-not (Test-Path -LiteralPath $saeSevenZip)) { throw '7-Zip is required; update $saeSevenZip to the installed path.' }
$saeReleases = @(
    @{Year=2022;Bytes=39096413;Url='https://datosabiertos.mineduc.cl/wp-content/uploads/2023/03/SAE_2022.rar'},
    @{Year=2023;Bytes=32866752;Url='https://datosabiertos.mineduc.cl/wp-content/uploads/2024/04/SAE-2023.rar'},
    @{Year=2024;Bytes=32230475;Url='https://datosabiertos.mineduc.cl/wp-content/uploads/2025/04/SAE_2024.rar'},
    @{Year=2025;Bytes=30633350;Url='https://datosabiertos.mineduc.cl/wp-content/uploads/2026/03/SAE_2025.rar'}
)
New-Item -ItemType Directory -Force -Path $saeArchiveRoot | Out-Null
foreach ($saeRelease in $saeReleases) {
    $saeArchive = Join-Path $saeArchiveRoot ('SAE_' + $saeRelease.Year + '.rar')
    if (-not (Test-Path -LiteralPath $saeArchive)) {
        Invoke-WebRequest -Uri $saeRelease.Url -OutFile $saeArchive -TimeoutSec 180
    }
    if ((Get-Item -LiteralPath $saeArchive).Length -ne $saeRelease.Bytes) {
        throw ('Unexpected archive size for ' + $saeRelease.Year + '; inspect the official release before proceeding.')
    }
    $saeTarget = Join-Path $saeSourceRoot $saeRelease.Year
    & $saeSevenZip x $saeArchive ('-o' + $saeTarget) '-ir!A1*' '-ir!ER*Admis*A1*' '-ir!Novedades*' -aos
    if ($LASTEXITCODE -ne 0) { throw ('Extraction failed for ' + $saeRelease.Year) }
}
$saeReleases | ForEach-Object { [PSCustomObject]$_ } | Export-Csv -Path (Join-Path $saeSourceRoot 'download_manifest.csv') -NoTypeInformation
