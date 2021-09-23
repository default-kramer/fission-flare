# Assumes that the current directory is the project root (one level up from this script).
# For local testing:
#   powershell -ExecutionPolicy Unrestricted .\dist\distribute-windows.ps1

# run : Stops execution if given command does not exit with code 0.
function run () {
  & $args[0] ($args | select -skip 1)
  if ($LASTEXITCODE -ne 0) {
    throw "Command failed: $args"
  }
}

echo "entered distribute-windows.ps1"
echo "running raco exe"
run raco exe -o fission-flare.exe src\ui\main-frame.rkt
echo "running raco distribute"
run raco distribute dist\windows fission-flare.exe
echo "creating zip"
Add-Type -assembly system.io.compression.filesystem
[io.compression.zipfile]::CreateFromDirectory("dist\windows", "dist\fission-flare-windows.zip")
