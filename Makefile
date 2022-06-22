# -*- indent-tabs-mode: t; -*-

# override to use something like, say, a local version of remacs
EMACS ?= emacs

all: upgrade-emacs-macos install-aero-macos

build-macos: build-emacs-macos install-aero-macos

build-emacs-macos:
	brew install coreutils
	brew tap d12frosted/emacs-plus
	# NOTE: dbus isn't working on M1 yet.
	brew install emacs-plus@29 --with-modern-sexy-v1-icon --with-native-comp --with-xwidgets
	ln -sf /opt/homebrew/opt/emacs-plus@29/Emacs.app /Applications

build-emacs-macos-stable:
	brew install coreutils
	brew tap d12frosted/emacs-plus
	brew install emacs-plus@28 --with-modern-sexy-v1-icon --with-native-comp --with-xwidgets
	ln -sf /opt/homebrew/opt/emacs-plus@28/Emacs.app /Applications

remove-emacs-macos:
	brew uninstall emacs-plus@29 || true

upgrade-emacs-macos: remove-emacs-macos build-emacs-macos

install-aero-macos:
	osacompile -o bin/Emacs\ \(Aero\).app bin/aero-emacs.osx.applescript
	cp etc/logo/Emacs.icns bin/Emacs\ \(Aero\).app/Contents/Resources/applet.icns
	[ -s /Applications/Emacs\ \(Aero\).app ] && rm -rf /Applications/Emacs\ \(Aero\).app
	mv bin/Emacs\ \(Aero\).app /Applications/

requirements-linux:
	./bin/build/linux-requirements.zsh

build-emacs-linux: requirements-linux
	./bin/build/linux.zsh

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
	# opam install ocaml-lsp-server
	# nix-env -i rnix-lsp
