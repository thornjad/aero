#!/usr/bin/env zsh

echo "Sudo is required for apt usage"
sudo -v

# Keep alive existing `sudo` time stamp until the script has finished.
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

sudo apt update

libs=(
  build-essential
  git
  libgccjit0
  libgif-dev
  libgnutls30
  libgtk-4-dev
  libjansson-dev
  libjpeg-dev
  libncurses-dev
  libpng-dev
  libtiff5-dev
  libxml2-dev
  libxpm-dev
)
sudo apt install ${libs[@]}
