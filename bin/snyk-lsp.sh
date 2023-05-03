#!/bin/bash
# This file allows you to download the latest language server, which is helpful for integration into non-managed Editors and IDEs.
# Currently, these might be any editor that does not have a downloader built by Snyk and thus needs to download
# and update the language server regularly, and this script allows this for system administrators and users.
# Snyk recommends always using the latest version of the language server.

set -e
OS=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m | tr '[:upper:]' '[:lower:]')
if [[ $ARCH == "x86_64" ]]; then
  ARCH="amd64"
fi
if [[ $ARCH == "aarch64" ]]; then
  ARCH="arm64"
fi
PROTOCOL_VERSION=3
VERSION=$(curl https://static.snyk.io/snyk-ls/$PROTOCOL_VERSION/metadata.json | jq .version | sed -e s/\"//g)
wget -O ~/bin/snyk-ls "https://static.snyk.io/snyk-ls/$PROTOCOL_VERSION/snyk-ls_${VERSION}_${OS}_${ARCH}"
chmod +x ~/bin/snyk-ls
