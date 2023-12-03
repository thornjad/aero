#!/usr/bin/env zsh

#################################
# Build script for Emacs on Linux
#
# This downloads the latest development version of Emacs into a temp directory, then builds it. The
# temp directory will be removed when the script exits.
#################################

echo "AERO --- Creating temporary work directory"
WORK_DIR=`mktemp -d`
if [[ ! "$WORK_DIR" || ! -d "$WORK_DIR" ]]; then
  echo "AERO --- Could not create temporary work directory"
  exit 1
fi
echo "AERO --- Temporary work dir: ${WORK_DIR}"

function cleanup() {
  rm -rf "$WORK_DIR"
}

# on exit signal, clean up
trap cleanup EXIT

### main

echo "AERO --- Installing requirements"
echo "AERO --- Sudo is required for apt usage"
sudo -v

# Keep alive existing `sudo` time stamp until the script has finished.
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

sudo apt update

libs=(
  autoconf
  build-essential
  git
  gnutls-bin
  libgccjit0
  libgccjit-12-dev
  libgif-dev
  libgnutls30
  libgtk-3-dev
  libgtk-4-dev
  libjansson-dev
  libjpeg-dev
  libncurses-dev
  libpng-dev
  libtiff5-dev
  libtree-sitter-dev
  libxml2-dev
  libxpm-dev
  texinfo
)
sudo apt install -y ${libs[@]}
echo "AERO --- Done with requirements"

echo "AERO --- Getting Emacs source"
git clone https://git.savannah.gnu.org/git/emacs.git "$WORK_DIR" || { echo "AERO --- Failed to clone repository"; exit 1; }
cd "$WORK_DIR"

echo "AERO --- Configuring Emacs build in directory:"
pwd

# Configure our installation
#
# See https://git.savannah.gnu.org/cgit/emacs.git/tree/plain/INSTALL for latest info
#
# --with-native-compilation: enables ahead-of-time native compilation [this is SLOW up front]
# --with-json: enables native JSON support
# --with-threads: enables support for threads for elisp libraries
# --with-compress-install: compresses installation files to save space
# --with-modules: enables dynamic modules
# --with-tree-sitter: enables tree-sitter support
# --with-gnutls=ifavailable: enables gnutls support if they're already available
# --without-mailutils: disables mailutils support which we don't use
# --without-pop: disables pop support, which is insecure and unused
# CFLAGS: enables CPU optimizations, using native architecture

./autogen.sh && ./configure --with-native-compilation=aot --with-json --with-threads --with-compress-install --with-modules --with-tree-sitter --with-gnutls=ifavailable --without-mailutils --without-pop CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer"
echo "AERO --- Building Emacs"
make -j12
echo "AERO --- Installing Emacs"
sudo make install
echo "AERO --- Cleaning up"
cleanup()
echo "AERO --- Done"
