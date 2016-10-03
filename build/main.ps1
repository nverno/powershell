<# 
.SYNOPSIS
 Write hash tables with metadata for powershell objects.
#>

Param($outfile)

if ($outfile -eq $null) {
    $outfile = [System.IO.Path]::GetFullPath("$PSScriptroot\posh-data.el")
} else {
    $outfile = [System.IO.Path]::GetFullPath("$outfile")
}

$helpers = [System.IO.Path]::GetFullPath("$PSscriptroot\helpers.ps1")
. $helpers

Write-EmacsHash | ac $outfile
