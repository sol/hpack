#!/bin/bash
set -o nounset
set -o errexit

dst="$HOME/.local/bin"
hpack="$dst/hpack"

mkdir -p "$dst"

os="${1:-${TRAVIS_OS_NAME:-linux}}"
url=$(curl -sSL https://api.github.com/repos/sol/hpack/releases/latest | jq -r ".assets[] | select(.name | test(\"$os\")) | .browser_download_url")

echo "Downloading $url"

curl -sSL "$url" | gunzip > "$hpack.tmp"
chmod +x "$hpack.tmp"
mv "$hpack.tmp" "$hpack"

echo "Installed hpack to $hpack"
