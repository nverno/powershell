<# 
.SYNOPSIS
  Generate metadata with command syntax for emacs eldoc.
.PARAMETER outfile
  Path to output file, defaults to $PSScriptroot\eldoc.el
.PARAMETER overwrite
  If non-nil overwrites output file.
.NOTES
  Get-Signature from powershell-mode.el on emacs-wiki.
.LINK
  https://www.emacswiki.org/emacs/PowerShell-Mode.el
#>

Param($outfile = $null, 
      $overwrite = $null)

if ($outfile -eq $null) {
    $outfile = [System.IO.Path]::GetFullPath("$PSScriptroot\eldoc-data.el")
} else {
    $outfile = [System.IO.Path]::GetFullPath("$outfile")
}

if (($overwrite -eq $null) -and (Test-Path $outfile)) {
    Write-Error "$outfile already exists" -ErrorAction "Stop"
}

function Get-Signature ($Cmd) {
    if ($Cmd -is [Management.Automation.PSMethod]) {
        $List = @($Cmd)
    }
    elseif ($Cmd -isnot [string]) {
        throw ("Get-Signature {<method>|<command>}`n" +
               "'$Cmd' is not a method or command")
    }
    else {
        $List = @(Get-Command $Cmd -ErrorAction SilentlyContinue)
    }
    if (!$List[0] ) {
        "Unable to open $Cmd"
    } else {
        foreach ($O in $List) {
            switch -regex ($O.GetType().Name) {
                'AliasInfo' {
                    Get-Signature ($O.Definition)
                }
                # not sure what to do with ExternalScript
                '(Cmdlet|ExternalScript)Info' {
                    $O.Definition
                }        
                'F(unction|ilter)Info'{
                    if ($O.Definition -match '^param *\(') {
                        $t = [Management.Automation.PSParser]::tokenize($O.Definition,
                                                                        [ref]$null)
                        $c = 1;$i = 1
                        while($c -and $i++ -lt $t.count) {
                            switch ($t[$i].Type.ToString()) {
                                GroupStart {$c++}
                                GroupEnd   {$c--}
                            }
                        }
                        # needs parsing
                        $O.Definition.substring(0,$t[$i].start + 1)
                    } 
                    else { $O.Name }
                }
                'PSMethod' {
                    foreach ($t in @($O.OverloadDefinitions)) {
                        while (($b=$t.IndexOf('`1[[')) -ge 0) {
                            $t=$t.remove($b,$t.IndexOf(']]')-$b+2)
                        }
                        $t
                    }
                }
            }
        }
    }
}

function Munge-Eldoc ($str) {
    ($str.Replace('\', '\\').`
      Replace('"', '\"') -replace '\[|\]|<.*?>','') -replace ' +', ' '
}

Get-Command Get-Process |
  ?{$_.CommandType -ne 'Alias' -and $_.Name -notlike '*:'} |
  %{$_.Name} |
  sort |
  %{("(puthash ""$($_.Replace('\', '\\'))"" " +
     "'((sig . ""$(Get-Signature $_ | %{ Munge-Eldoc $_ })"")").`
       Replace('`r`n"")"', '"")"') + "(pars . " +
    [System.String]::Join(" ", $(Get-Command $_ | 
      select -ExpandProperty Parameters | Select -expandProperty Keys)) +
    "))"}

# | ac $outfile
  # %{Format-Eldoc $_} | ac $outfile
