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
    $outfile = [System.IO.Path]::GetFullPath("$PSScriptroot\eldoc.el")
} else {
    $outfile = [System.IO.Path]::GetFullPath("$outfile")
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
        continue
        # throw "Command '$Cmd' not found"
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

# get-command |
#   ?{$_.CommandType -ne 'Alias' -and $_.Name -notlike '*:'} |
#   %{$_.Name} |
#   sort |
#   %{("(set (intern ""$($_.Replace('\','\\'))"" posh-eldoc-obarray)" +
#      " ""$(Get-Signature $_|%{$_.Replace('\','\\').Replace('"','\"')})"")"
#     ).Replace("`r`n"")",""")")} | ac $outfile

