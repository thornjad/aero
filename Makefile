# -*- indent-tabs-mode: t; -*-

# override to use something like, say, a local version of remacs
EMACS ?= emacs
EMACS_BUILD_DIR ?= ~/lib/emacs/

all: upgrade-emacs-macos nongnu-elpa install-aero-macos

build-macos: build-emacs-macos install-aero-macos

# required for Aero, emacs-plus handles the actual Emacs dependencies
macos-reqs:
	brew update
	brew install coreutils git-delta
	brew tap d12frosted/emacs-plus

build-emacs-cask-stable: macos-reqs
	brew install --cask emacs

build-emacs-cask: macos-reqs
	brew install --cask emacs-nightly

build-emacs-macos: macos-reqs
	# NOTE: dbus isn't working on M1 yet.
	brew install emacs-plus@29 --with-modern-sexy-v1-icon --with-native-comp --with-xwidgets
	ln -sf /opt/homebrew/opt/emacs-plus@29/Emacs.app /Applications

# for when libgccjit breaks every few months
build-emacs-macos-without-native-comp: macos-reqs
	# NOTE: dbus isn't working on M1 yet.
	brew install emacs-plus@29 --with-modern-sexy-v1-icon --with-xwidgets
	ln -sf /opt/homebrew/opt/emacs-plus@29/Emacs.app /Applications

build-emacs-macos-stable: macos-reqs
	brew install emacs-plus@28 --with-modern-sexy-v1-icon --with-native-comp --with-xwidgets
	ln -sf /opt/homebrew/opt/emacs-plus@28/Emacs.app /Applications

build-emacs-macos-stable-without-native-comp: macos-reqs
	brew install emacs-plus@28 --with-modern-sexy-v1-icon --with-xwidgets
	ln -sf /opt/homebrew/opt/emacs-plus@28/Emacs.app /Applications

remove-emacs-macos:
	brew uninstall emacs-plus@29 || true

remove-emacs-macos-stable:
	brew uninstall emacs-plus@28 || true

upgrade-emacs-macos: remove-emacs-macos build-emacs-macos

install-aero-macos:
	osacompile -o bin/Emacs\ \(Aero\).app bin/aero-emacs.osx.applescript
	cp etc/logo/Emacs.icns bin/Emacs\ \(Aero\).app/Contents/Resources/applet.icns
	[ -s /Applications/Emacs\ \(Aero\).app ] && rm -rf /Applications/Emacs\ \(Aero\).app
	mv bin/Emacs\ \(Aero\).app /Applications/

build-emacs-linux: nongnu-elpa
	./bin/build/linux-requirements.zsh
	cd ${EMACS_BUILD_DIR} && \
	git stash -m "Emacs build autostash" && \
	git pull --rebase && \
	./autogen.sh && \
	./configure --with-native-compilation --with-json --with-threads --with-compress-install --with-modules --with-gnutls=ifavailable --without-mailutils CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer" && \
	make -j12 && \
	sudo make install

.PHONY: nongnu-elpa
nongnu-elpa:
	# nongnu-elpa is corrupted somehow, this fixes it by cloning without fsck whether or not it's already there
	rm -rf ~/.config/emacs/straight/repos/nongnu-elpa/
	mkdir -p ~/.config/emacs/straight/repos/
	git clone https://git.savannah.gnu.org/git/emacs/nongnu.git ~/.config/emacs/straight/repos/nongnu-elpa --config transfer.fsckobjects=false --config receive.fsckobjects=false --config fetch.fsckobjects=false

install-dependencies: install-lsp-servers
	npm i -g sass-lint eslint tern

update-dependencies: install-dependencies

update-lsp-servers: install-lsp-servers

install-lsp-servers:
	npm i -g bash-language-server @types/node
	npm i -g @angular/language-service@next typescript @angular/language-server typescript-language-server eslint
	npm i -g emmet-ls
	pip3 install python-lsp-server pyls-mypy black pyls-black pyls-isort flake8 jedi
	pip3 install "ptvsd>=4.2"
	rustup component add rls rust-analysis rust-src
	brew install clojure-lsp/brew/clojure-lsp-native
	# opam install ocaml-lsp-server
	# nix-env -i rnix-lsp
