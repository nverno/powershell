<# 
.SYNOPSIS
Helper functions to provide completion/eldoc/help for powershell-mode.  The write 
functions produce two hash tables, posh-functions and posh-variables.
#>

# list of all about* topics
function Get-EmacsTopics () {
    Get-Help about_* | %{$_.Name.substring(6)}
}

function Get-EmacsDrives () {
    @(Get-ChildItem env: | %{$_.Name}),
    @(Get-ChildItem variable: | %{$_.Name}),
    @(Get-ChildItem alias: | %{$_.Name})
}

# return possible variables for prefix
function Get-EmacsVariable ($prefix) {
    Get-Variable -Name $prefix | %{$_.Name}
}

function Get-EmacsHelp ($func) {
    Get-Help $func | %{$_.Description}
}

# format list of paramters for $cmd
function Format-EmacsParams ($cmd) {
    if ($cmd.Name -notLike '*:') {
        $pars = Get-Help $cmd | Select-Object -ExpandProperty Syntax | 
          Select-Object -ExpandProperty syntaxItem |
          Select-Object -ExpandProperty Parameter | 
          Select-Object -uniq -ExpandProperty Name 
        if ($pars -ne $null) {
            '"' + [System.String]::Join('" "', $pars) + '"'
        } else {""}
    }
}

# output mappings for alias(es) -> definitions
function Write-EmacsAlias($alias, $hash="posh-functions") {
    Get-ChildItem alias:$alias | 
      %{("(puthash ""$($_.Name)"" "+
         "'((alias . ""$($_.Definition)"") "+
         "(type . ""Alias<$($_.Definition)>"")) $hash)").Replace('\', '\\')}
}

# format function(s) info as hash
function Write-EmacsFunction ($hash = "posh-functions") {
    Process {
        if ($_.CommandType -eq "Alias") {
            Write-EmacsAlias $_ $hash
        } elseif ($_.Name -notLike '*:') {
            %{("(puthash ""$($_.Name.Replace('\','\\'))"" "+
               "'((type . ""$($_.CommandType)"") "+
               "(synopsis . "+
               """$(Get-Help $_|%{$_.Synopsis.Replace('\','\\').Replace('`"','\`"')})"")"+
               "(params . ($(Format-EmacsParams $_)))) $hash)")}
        }
    }
}

# format variables
function Write-EmacsVariable ($var, $hash="posh-variables") {
    Get-ChildItem variable:$var | 
      %{"(puthash ""$($_.Name)"" "+
        "'((type . ""Variable"") (value . ""$_.Value"") "+
        "(annot . ""$($_.Visibility)"")) $hash)"}
}

function Format-EmacsEnv($var) {
    $res = $var.Value
    if ($res.length -gt 70) {
        $res = $res.substring(0, 67) + "..."
    } 
    $res.replace('\','\\').replace('"','\"')
}

function Write-EmacsEnv ($var, $hash="posh-env") {
    Get-ChildItem env:$var |
      %{"(puthash ""$($_.Name.Replace('\','\\').Replace('`"','\`"'))"" "+
        "'((type . ""Env"") (value . ""$(Format-EmacsEnv $_)"") (annot . `"Env`")) $hash)"}
}

# init hash
function Write-EmacsHash () {
    Write-EmacsVariable $null
    Write-EmacsAlias
    Write-EmacsEnv
    Get-Command | Write-EmacsFunction
}
