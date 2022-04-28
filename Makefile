# -*- indent-tabs-mode: t; -*-

# override to use something like, say, a local version of remacs
EMACS ?= emacs

all: update-packages compile-packages

build-macos: build-emacs-macos install-aero-macos

build-emacs-macos:
	brew install coreutils
	brew tap d12frosted/emacs-plus
	# NOTE: dbus isn't working on M1 yet.
	brew install emacs-plus@29 --with-modern-sexy-v1-icon --with-native-comp --with-xwidgets

install-aero-macos:
	osacompile -o bin/Emacs\ \(Aero\).app bin/aero-emacs.osx.applescript
	cp etc/logo/Emacs.icns bin/Emacs\ \(Aero\).app/Contents/Resources/applet.icns
	[ -s /Applications/Emacs\ \(Aero\).app ] && rm -rf /Applications/Emacs\ \(Aero\).app
	mv bin/Emacs\ \(Aero\).app /Applications/

requirements-linux:
	./bin/build/linux-requirements.zsh

build-emacs-linux: requirements-linux
	./bin/build/linux.zsh

# update all submodule packages, byte-compile submodule packages, then update
# *ELPA packages. The *ELPA update is separated from the byte-compile step
# because GNU ELPA goes down a lot, and that shouldn't block submodules. When
# running auto-package-update, we're still using the system-installed Emacs
# 26.2, so we need to turn of TLS 1.3. Eventually this will use Remacs and the
# issue will go away.
update-packages:
	git submodule update --init --recursive --rebase --remote

compile-packages:
	$(EMACS) -batch -l ~/.config/emacs/init.el --eval '(package-initialize)' -f batch-byte-compile ./lib/packages/*/*(!-test).el

install-dependencies: install-lsp-servers
	npm i -g sass-lint eslint tern

install-lsp-servers:
	npm i -g bash-language-server @types/node
	npm i -g @angular/language-service@next typescript @angular/language-server typescript-language-server tslint
	npm install -g emmet-ls
	pip3 install python-lsp-server pyls-mypy black pyls-black pyls-isort flake8 jedi
	pip3 install "ptvsd>=4.2"
	rustup component add rls rust-analysis rust-src
	# opam install ocaml-lsp-server
	# nix-env -i rnix-lsp
