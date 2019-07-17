# -*- indent-tabs-mode: t; -*-

# update all submodule packages, byte-compile submodule packages, then update
# *ELPA packages. The *ELPA update is separated from the byte-compile step
# because GNU ELPA goes down a lot, and that shouldn't block submodules.
update-packages:
	git submodule update --init --recursive --rebase --remote
	emacs -batch --eval '(package-initialize)' -f batch-byte-compile ./lib/packages/*/
	emacs -batch --eval '(progn(package-initialize)(auto-package-update-now))'
