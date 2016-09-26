# Store encrypted password to elevate powershell scripts
# in the future
$path = $env:HOME
write-host $path

# Read password
$passwd = Read-Host "Enter password" -AsSecureString
$encpwd = ConvertFrom-SecureString $passwd
$encpwd > $path\password.bin

