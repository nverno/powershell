<# 
.SYNOPSIS
  Generate metadata with command syntax for emacs eldoc.
.PARAMETER outfile
  Path to output file, defaults to $PSScriptroot\eldoc.el
.PARAMETER overwrite
  If non-nil overwrites output file.
.NOTES
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

function Format-Eldoc ($cmd, $obarray="powershell-eldoc-obarray") {
    Process {
        if ($_.Name -notLike '*:') {
            $name = $_.Name.ToLower().Replace('\', '\\')
            $pars = Get-Help $_ | Select -ExpandProperty Syntax | 
              Select -ExpandProperty syntaxItem |
              Select -ExpandProperty Parameter | Select -uniq -ExpandProperty Name 
            $pars = if ($pars -ne $null) {
                '"' + [System.String]::Join('" "', $pars) + '"'
            } else {""}
            "(set (intern ""$name"" $obarray) '($pars))"
        }
    }
}

# generate parameters for all commands
Get-Command | Format-Eldoc $_ | ac $outfile

# function Get-Signature ($Cmd) {
#     if ($Cmd -is [Management.Automation.PSMethod]) {
#         $List = @($Cmd)
#     }
#     elseif ($Cmd -isnot [string]) {
#         throw ("Get-Signature {<method>|<command>}`n" +
#                "'$Cmd' is not a method or command")
#     }
#     else {
#         $List = @(Get-Command $Cmd -ErrorAction SilentlyContinue)
#     }
#     if (!$List[0] ) {
#         "Unable to open $Cmd"
#     } else {
#         foreach ($O in $List) {
#             switch -regex ($O.GetType().Name) {
#                 'AliasInfo' {
#                     Get-Signature ($O.Definition)
#                 }
#                 # not sure what to do with ExternalScript
#                 '(Cmdlet|ExternalScript)Info' {
#                     $O.Definition
#                 }        
#                 'F(unction|ilter)Info'{
#                     if ($O.Definition -match 'param *\(') {
#                         $t = [Management.Automation.PSParser]::tokenize($O.Definition,
#                                                                         [ref]$null)
#                         $c = 1;$i = 1
#                         while($c -and $i++ -lt $t.count -and $t[$i] -ne $null) {
#                             switch ($t[$i].Type.ToString()) {
#                                 GroupStart {$c++}
#                                 GroupEnd   {$c--}
#                             }
#                         }
#                         # needs parsing
#                         $O.Definition.substring(0,$t[$i].start + 1)
#                     } 
#                     else { $O.Name }
#                 }
#                 'PSMethod' {
#                     foreach ($t in @($O.OverloadDefinitions)) {
#                         while (($b=$t.IndexOf('`1[[')) -ge 0) {
#                             $t=$t.remove($b,$t.IndexOf(']]')-$b+2)
#                         }
#                         $t
#                     }
#                 }
#             }
#         }
#     }
# }

# function Clean-Signature ($str) {
#     ($str.Replace('\', '\\').Replace('"', '\"') `
#       -replace '\A[ \n\r]+|[\n\r ]+$|\[|\]|<.*?>','') -replace ' +', ' ' `
#       -replace '[\n\r]+', '\n'
# }

# function Format-Eldoc ($cmd, $obarray="powershell-eldoc-obarray") {
#     Process {
#         if ($_.CommandType -ne 'Alias' -and $_.Name -notLike '*:') {
#             $name = $_.Name
#             "(set (intern ""$($name.Replace('\','\\'))"" $obarray)" +
#             " ""$(Get-Signature $name | %{Clean-Signature($_ -replace $name, '')})"")"
#         }
#     }
# }

# Get-Command Get-Process |
#   ?{$_.CommandType -ne 'Alias' -and $_.Name -notlike '*:'} |
#   %{$_.Name} |
#   sort |
#   %{("(puthash ""$($_.Replace('\', '\\'))"" " +
#      "'((sig . ""$(Get-Signature $_ | %{ Clean-Eldoc $_ })"")").`
#        Replace('`r`n"")"', '"")"') + "(pars . " +
#     [System.String]::Join(" ", $(Get-Command $_ | 
#       select -ExpandProperty Parameters | Select -expandProperty Keys)) +
#     "))"}

# Get-Command |
#   ?{$_.CommandType -ne 'Alias' -and $_.Name -notlike '*:'} |
#   %{$_.Name} |
#   sort |
#   %{("(set (intern ""$($_.Replace('\','\\'))"" powershell-eldoc-obarray)" +
#      " ""$(Get-Signature $_|%{$_.Replace('\','\\').Replace('"','\"')})"")").`
#        Replace("`r`n"")",""")")} | ac $outfile
