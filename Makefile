# -*- indent-tabs-mode: t; -*-

# update all submodule packages, byte-compile submodule packages, then update
# *ELPA packages. The *ELPA update is separated from the byte-compile step
# because GNU ELPA goes down a lot, and that shouldn't block submodules. When
# running auto-package-update, we're still using the system-installed Emacs
# 26.2, so we need to turn of TLS 1.3. Eventually this will use Remacs and the
# issue will go away.
update-packages:
	git submodule update --init --recursive --rebase --remote
	emacs -batch --eval '(package-initialize)' -f batch-byte-compile ./lib/packages/*/
	emacs -batch --eval '(progn(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")(package-initialize)(auto-package-update-now))'
