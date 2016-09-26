# Read stored password and run script without UAC prompt or window popup
# @@FIXME: should try/catch when trying to read password
param (
    [switch] $asAdmin,
    [string] $inFile,
    [string] $pswdLoc
)

function run-no-prompt {
    Param([bool] $asAdmin,
          [string] $inFile
          [string] $pswdLoc)
    
    if ($asAdmin) {
        $pdir = (Split-Path $SCRIPT:MyInvocation.MyCommand.Path -parent)
        $hdir = $env:HOME
        
        # check that password.bin exists
        # $pswdLoc = (Join-Path $env:HOME "password.bin")
        if (![System.IO.File]::Exists($pswdLoc)) {
            $pscript = (Join-Path $pdir "store-password.ps1")
            Start-Process powershell -Wait -ArgumentList '-File', $pscript
        }
        
        echo "*** Running as admin ***"
        # read encrypted password
        $encpwd = Get-Content $pswdLoc
        $passwd = ConvertTo-SecureString $encpwd
        $user = $env:UserName
        $cred = new-object System.Management.Automation.PSCredential `
          $user,$passwd
        Start-Process powershell -WindowStyle hidden -Cred $cred -ArgumentList `
          '-File', $inFile
    } else {
        Start-Process powershell -WindowStyle hidden '-File', $inFile
    }
}

# run input
run-no-prompt -asAdmin $asAdmin -inFile $inFile -pswdLoc $pswdLoc

# run-no-prompt -asAdmin $true -inFile `
#   "C:/home/.emacs.d/etc/config/win/ps/scratch/path.ps1"

