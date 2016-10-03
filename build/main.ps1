<# 
.SYNOPSIS
 Write hash tables with metadata for powershell objects.
#>

Param($outfile,
      $append=$true)

if ($outfile -eq $null) {
    $outfile = [System.IO.Path]::GetFullPath("$PSScriptroot\posh-data.el")
} else {
    $outfile = [System.IO.Path]::GetFullPath("$outfile")
}

$helpers = [System.IO.Path]::GetFullPath("$PSscriptroot\helpers.ps1")
. $helpers

if (!(Test-Path $outfile) -or $append) {
    Write-EmacsHash | ac $outfile
}
