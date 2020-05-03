# This script is invoked from my Appveyor commands
# It bootstraps to install stack and run the tests
$ErrorActionPreference = "Stop"

# If there is an error, we stop (thanks to the preference above). However, when running commands:
#
# * Writing to stderr is an error, despite the fact stack puts its progress messages there.
#   We fix that by running inside 'cmd' and redirecting stderr to stdout.
#
# * Giving a non-zero exit code is NOT an error. We fix that by testing LASTERRORCODE after each command.

$env:LC_ALL='C.UTF-8'
chcp 65001

# Make sure stack.exe is on PATH, even if we change directory
$env:PATH += ";$PWD"

# Short Stack root to avoid overflowing path lengths
$env:STACK_ROOT = 'C:\sr'

# Workaround https://github.com/haskell/cabal/issues/5386
$env:TMP = 'C:\tmp'
New-Item -ItemType directory -Path C:\tmp

# Workaround https://github.com/fpco/stackage-server/issues/290
# Invoke-WebRequest 'https://www.stackage.org/stack/windows-x86_64' -OutFile 'stack.zip'
Invoke-WebRequest 'https://github.com/commercialhaskell/stack/releases/download/v2.1.3/stack-2.1.3-windows-x86_64.zip' -OutFile 'stack.zip'
7z x -y stack.zip stack.exe
if ($LASTEXITCODE -ne 0) {exit 1}

Invoke-WebRequest 'https://downloads.haskell.org/~cabal/cabal-install-2.4.1.0/cabal-install-2.4.1.0-x86_64-unknown-mingw32.zip' -OutFile 'cabal.zip'
7z x -y cabal.zip cabal.exe
if ($LASTEXITCODE -ne 0) {exit 1}
cmd /c '.\cabal v1-update 2>&1'
if ($LASTEXITCODE -ne 0) {exit 1}

# If powershell ever sees anything on stderr it decides to fail
# Therefore we use cmd to redirect stderr to stdout before powershell sees it
if (!(Test-Path "stack.yaml")) {
    cmd /c '.\stack init --resolver=nightly --ignore-subdirs 2>&1'
    if ($LASTEXITCODE -ne 0) {exit 1}
}
# Required to get Weeder working with the latest Stack
Add-Content "stack.yaml" "`nghc-options: {`"`$locals`": -ddump-to-file -ddump-hi}"

cmd /c '.\stack setup 1>&2 2>&1 > nul'
if ($LASTEXITCODE -ne 0) {exit 1}

$HASKELL_DEPENDENCIES = $env:HASKELL_DEPENDENCIES
if ($HASKELL_DEPENDENCIES -ne '') {
    cmd /c "echo | chcp 65001 && .\stack --no-terminal install $HASKELL_DEPENDENCIES 2>&1"
    if ($LASTEXITCODE -ne 0) {exit 1}
}

cmd /c 'echo | chcp 65001 && .\stack --no-terminal build --test --bench --ghc-options=-rtsopts 2>&1'
if ($LASTEXITCODE -ne 0) {exit 1}
