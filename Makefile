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
	brew install emacs-plus@30 --with-modern-sexy-v1-icon --with-native-comp --with-xwidgets --with-tree-sitter
	ln -sf /opt/homebrew/opt/emacs-plus@30/Emacs.app /Applications

# for when libgccjit breaks every few months
build-emacs-macos-minimal: macos-reqs
	# NOTE: dbus isn't working on M1 yet.
	brew install emacs-plus@30 --with-modern-sexy-v1-icon --with-xwidgets
	ln -sf /opt/homebrew/opt/emacs-plus@30/Emacs.app /Applications

build-emacs-macos-stable: macos-reqs
	brew install emacs-plus@29 --with-modern-sexy-v1-icon --with-native-comp --with-xwidgets --with-tree-sitter
	ln -sf /opt/homebrew/opt/emacs-plus@29/Emacs.app /Applications

build-emacs-macos-stable-minimal: macos-reqs
	brew install emacs-plus@29 --with-modern-sexy-v1-icon --with-xwidgets
	ln -sf /opt/homebrew/opt/emacs-plus@29/Emacs.app /Applications

remove-emacs-macos:
	brew uninstall emacs-plus@30 || true

remove-emacs-macos-stable:
	brew uninstall emacs-plus@29 || true

upgrade-emacs-macos: remove-emacs-macos build-emacs-macos

install-aero-macos:
	osacompile -o bin/Emacs\ \(Aero\).app bin/aero-emacs.osx.applescript
	cp etc/logo/Emacs.icns bin/Emacs\ \(Aero\).app/Contents/Resources/applet.icns
	[ -s /Applications/Emacs\ \(Aero\).app ] && rm -rf /Applications/Emacs\ \(Aero\).app || true
	mv bin/Emacs\ \(Aero\).app /Applications/

clean-aero-macos:
	rm -rf /Applications/Emacs\ \(Aero\).app

build-emacs-linux: nongnu-elpa
	./bin/build/linux.zsh

install-aero-linux:
	mkdir -p ~/.local/share/applications/
	cp ./bin/aero-emacs.desktop ~/.local/share/applications/

linux: build-emacs-linux install-aero-linux

.PHONY: nongnu-elpa
nongnu-elpa:
	# nongnu-elpa is corrupted somehow, this fixes it by cloning without fsck whether or not it's already there
	rm -rf ~/.config/emacs/straight/repos/nongnu-elpa/
	mkdir -p ~/.config/emacs/straight/repos/
	git clone https://git.savannah.gnu.org/git/emacs/nongnu.git ~/.config/emacs/straight/repos/nongnu-elpa --config transfer.fsckobjects=false --config receive.fsckobjects=false --config fetch.fsckobjects=false

init: nongnu-elpa install-deps
	git submodule init
	git submodule update

clear-straight:
	rm -rf ./straight/

# Clear out packages and re-init
hard-init: clear-straight init

install-deps:
	# Continues even on failures. This lets us only install what the system can install, but can
	# swallow up errors
	npm i -g bash-language-server @types/node || true
	npm i -g @angular/language-service@next typescript @angular/language-server typescript-language-server eslint @elm-tooling/elm-language-server || true
	npm i -g emmet-ls vscode-json-languageserver || true
	gem install bundler prettier_print syntax_tree syntax_tree-haml syntax_tree-rbs && npm i -g prettier @prettier/plugin-ruby || true

	pip install python-lsp-server pyls-mypy pyls-black pyls-isort mypy ruff black "ptvsd>=4.2" || true

	rustup component add rls rust-analysis rust-src || true
	brew install clojure-lsp/brew/clojure-lsp-native || true
	gem install solargraph || true
	opam install ocaml-lsp-server || true
	nix-env -i rnix-lsp || true
