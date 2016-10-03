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
    @(gci env: | %{$_.Name}),
    @(gci variable: | %{$_.Name}),
    @(gci alias: | %{$_.Name})
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
        $pars = Get-Help $cmd | Select -ExpandProperty Syntax | 
          Select -ExpandProperty syntaxItem |
          Select -ExpandProperty Parameter | Select -uniq -ExpandProperty Name 
        if ($pars -ne $null) {
            '"' + [System.String]::Join('" "', $pars) + '"'
        } else {""}
    }
}

# output mappings for alias(es) -> definitions
function Write-EmacsAlias($alias) {
    gci alias:$alias | 
      %{("(puthash ""$($_.Name)"" "+
         "'((alias . ""$($_.Definition)"") "+
         "(type . ""Alias<$($_.Definition)>"")) posh-functions)").Replace('\', '\\')}
}

# format function(s) info as hash
function Write-EmacsFunction () {
    Process {
        if ($_.CommandType -eq "Alias") {
            Write-EmacsAlias $_
        } elseif ($_.Name -notLike '*:') {
            %{("(puthash ""$($_.Name.Replace('\','\\'))"" "+
               "'((type . ""$($_.CommandType)"") "+
               "(synopsis . "+
               """$(Get-Help $_|%{$_.Synopsis.Replace('\','\\').Replace('`"','\`"')})"")"+
               "(params . ($(Format-EmacsParams $_)))) posh-functions)")}
        }
    }
}

# format variables
function Write-EmacsVariable ($var) {
    gci variable:$var | 
      %{"(puthash ""$($_.Name)"" "+
        "'((type . ""Variable"") (value . ""$_.Value"") "+
        "(annot . ""$($_.Visibility)"") posh-variables)"}
}

function Format-EmacsEnv($var) {
    $res = $var.Value
    if ($res.length -gt 70) {
        $res = $res.substring(0, 67) + "..."
    } 
    $res.replace('\','\\').replace('"','\"')
}

function Write-EmacsEnv ($var) {
    gci env:$var |
      %{"(puthash ""$($_.Name.Replace('\','\\').Replace('`"','\`"'))"" "+
        "'((type . ""Env"") (value . ""$(Format-EmacsEnv $_)"") (annot . `"Env`")) posh-env)"}
}

# init hash
function Write-EmacsHash () {
    Write-EmacsVariable $null
    Write-EmacsAlias
    Write-EmacsEnv
    Get-Command | Write-EmacsFunction
}
