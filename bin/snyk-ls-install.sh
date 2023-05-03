#!/bin/bash

# Downloads the latest version of snyk-ls from the Snyk CDN and installs it to /usr/local/bin

# WARNING: This script requires the current PROTOCOL_VERSION, which changes without warning. Check
# here for LS_PROTOCOL_VERSION for the latest (at the bottom):
# https://github.com/snyk/snyk-ls/blob/main/.goreleaser.yaml

set -e
OS=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m | tr '[:upper:]' '[:lower:]')
if [[ $ARCH == "x86_64" ]]; then
  ARCH="amd64"
fi
if [[ $ARCH == "aarch64" ]]; then
  ARCH="arm64"
fi
PROTOCOL_VERSION=9
DESTINATION="/usr/local/bin/snyk-ls"
VERSION=$(curl https://static.snyk.io/snyk-ls/$PROTOCOL_VERSION/metadata.json | jq .version | sed -e s/\"//g)
DOWNLOAD_URL="https://static.snyk.io/snyk-ls/$PROTOCOL_VERSION/snyk-ls_${VERSION}_${OS}_${ARCH}"

echo "OS: $OS"
echo "Architecture: $ARCH"
echo "Protocol Version: $PROTOCOL_VERSION"
echo "Language Server version: $VERSION"
echo "Destination Path: $DESTINATION\n"
echo "Downloading from $DOWNLOAD_URL\n"
curl -L --progress-bar $DOWNLOAD_URL > $DESTINATION
chmod +x $DESTINATION
echo "\nInstalled Snyk LS $VERSION to $DESTINATION"
