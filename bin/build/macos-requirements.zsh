#!/usr/bin/env zsh

xcode-select --install

# Install homebrew if we don't already have it
if ! command -v brew </dev/null 2>&1; then
  echo "Installing Homebrew..."
  curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh
fi

brew update
brew upgrade

libs=(
  autoconf
  automake
  gcc
  git
  gnutls
  jansson
  jpeg
  libgccjit
  pkg-config
  texinfo
  python
  node
)
echo "Installing required libraries..."
brew install ${libs[@]}
echo "Requirements installed"
