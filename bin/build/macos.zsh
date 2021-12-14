#!/usr/bin/env zsh

# Copyright © 2021 Jade Michael Thornton

# Derived from https://github.com/mclear-tools/build-emacs-macos
# Patches from https://github.com/d12frosted/homebrew-emacs-plus

prompt_confirm() {
  while true; do
    read -r -n 1 -p "${1:-Continue?} [Y/n]: " REPLY
    case $REPLY in
      [yY]) echo ; return 0 ;;
      [nN]) echo ; return 1 ;;
      *) echo ; return 0 ;;
    esac
  done
}

# Exit if something goes wrong
set -e

killall -q -0 emacs && prompt_confirm "Cannot build Emacs while it's running; kill Emacs?" && killall -9 emacs

# variables
ROOT_DIR="`pwd`"
BUILD_DIR=/tmp/emacs-build

# Use homebrew libxml and pkgconfig
export LDFLAGS="-L/opt/homebrew/opt/libxml2/lib"
export CPPFLAGS="-I/opt/homebrew/opt/libxml2/include"
export PKG_CONFIG_PATH="/opt/homebrew/opt/libxml2/lib/pkgconfig"

# Clean out our dir if it exists
echo "Fetching Emacs from source..."
rm -rf ${BUILD_DIR}
mkdir ${BUILD_DIR}

# Clone it
cd ${BUILD_DIR}
git clone https://git.savannah.gnu.org/git/emacs.git .
git checkout master
git pull

# Set up dynamic variables

echo "Collecting information from git..."
REV=`git log -n 1 --no-color --pretty='format:%h' origin/master`
TIMESTAMP=`git log -n 1 --no-color --pretty='format:%at' origin/master`

cp ${ROOT_DIR}/emacs-version-git-commit.el ${BUILD_DIR}/
sed -e "s/@@GIT_COMMIT@@/$REV/" -i '' ${BUILD_DIR}/emacs-version-git-commit.el

# patches

DO_PATCHES=$(prompt_confirm "Apply patches?")
if [[ $DO_PATCHES -eq 0 ]]; then
  echo "Applying patches..."
  PATCH_LIST=`find ${ROOT_DIR}/patches/ -name '*.patch'`
  for f in ${PATCH_LIST}; do
    echo "Applying patch `basename $f`"
    patch -p1 -i $f
  done
fi

# System info

echo "Collecting system information..."
DAY=`date -u -r $TIMESTAMP +"%Y%m%d"`
ORIG=`grep ^AC_INIT configure.ac`
VNUM=`echo $ORIG | sed 's#^AC_INIT(\(.*\))#\1#; s/ //g' | cut -f2 -d,`
VERS="${DAY}_${REV}"
DESCR="Emacs_${VNUM}_${VERS}"

# autogen

echo "Generating build files..."
./autogen.sh

# compilation flags (clang is faster than gcc on mac)

CFLAGS="-g -O2"
export CC=clang
export OBJC=clang

# throw the version into info files

VERSION_FILES="
  nextstep/templates/Emacs.desktop.in
  nextstep/templates/Info-gnustep.plist.in
  nextstep/templates/Info.plist.in
  nextstep/templates/InfoPlist.strings.in"
for f in $VERSION_FILES; do
  sed -e "s/@version@/@version@ $VERS/" -i '' $f
done

# Configure

echo "Configuring Emacs..."

./configure \
  --with-ns \
  --with-modules \
  --with-gnutls \
  --with-native-compilation \
  --with-xwidgets \
  --with-json \
  --without-mailutils

# Now we build

prompt_confirm "Continue to building Emacs?" || exit 0
echo "Building Emacs version ${VNUM}_${VERS}..."

NCPU=$(getconf _NPROCESSORS_ONLN)
make bootstrap -j$NCPU | tee bootstrap-log.txt || exit 1 && make install -j$NCPU | tee build-log.txt

# Make into Mac app

echo "Creating MacOS App..."

# remove old Emacs
if command -v trash </dev/null 2>&1
then
  echo "Trashing old emacs..."
  trash /Applications/Emacs.app
else
  echo "Removing old emacs..."
  rm -rf /Applications/Emacs.app
fi

mv ${BUILD_DIR}/nextstep/Emacs.app /Applications

# install better icon
cp ${ROOT_DIR}/etc/logo/Emacs.icns /Applications/Emacs.app/Contents/Resources/Emacs.icns

# Copy over source code
cp -r ${BUILD_DIR}/src /Applications/Emacs.app/Contents/Resources/

# Now cleanup

echo "Cleaning up installation files"
LOG_DIR=${ROOT_DIR}/build-logs/${DESCR}
mkdir -p ${LOG_DIR}
mv ${BUILD_DIR}/config.log ${LOG_DIR}/config.log
mv ${BUILD_DIR}/build-log.txt ${LOG_DIR}/build-log.txt
mv ${BUILD_DIR}/bootstrap-log.txt ${LOG_DIR}/bootstrap-log.txt

prompt_confirm "Remove temp Emacs build source?" && rm -rf ${BUILD_DIR}

echo "Done!"
echo "Log files available at ${ROOT_DIR}/build-logs/"
