;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
	"Layer configuration:
This function should only modify configuration layer settings."
	(setq-default
	 ;; Base distribution to use. This is a layer contained in the directory
	 ;; `+distribution'. For now available distributions are `spacemacs-base'
	 ;; or `spacemacs'. (default 'spacemacs)
	 dotspacemacs-distribution 'spacemacs

	 ;; Lazy installation of layers (i.e. layers are installed only when a file
	 ;; with a supported type is opened). Possible values are `all', `unused'
	 ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
	 ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
	 ;; lazy install any layer that support lazy installation even the layers
	 ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
	 ;; installation feature and you have to explicitly list a layer in the
	 ;; variable `dotspacemacs-configuration-layers' to install it.
	 ;; (default 'unused)
	 dotspacemacs-enable-lazy-installation 'unused

	 ;; If non-nil then Spacemacs will ask for confirmation before installing
	 ;; a layer lazily. (default t)
	 dotspacemacs-ask-for-lazy-installation t

	 ;; If non-nil layers with lazy install support are lazy installed.
	 ;; List of additional paths where to look for configuration layers.
	 ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
	 dotspacemacs-configuration-layer-path '()

	 ;; List of configuration layers to load.
	 dotspacemacs-configuration-layers
	 '(vimscript
		 helm
		 emacs-lisp
		 neotree
		 sql
		 html
		 javascript
		 yaml
		 gtags
		 auto-completion
		 better-defaults
		 git
		 markdown
		 org
		 spell-checking
		 syntax-checking
		 ;; (version-control :variables
		 ;;										version-control-diff-tool 'git-gutter
		 ;;										version-control-diff-side 'left
		 ;;										version-control-global-margin t)
		 common-lisp
		 ;; spacemacs-purpose
		 (shell :variables
						shell-default-shell 'eshell
						shell-default-height 35
						shell-default-width 180
						shell-command-switch "-ic"
						shell-default-position 'top)
		 )

	 ;; List of additional packages that will be installed without being
	 ;; wrapped in a layer. If you need some configuration for these
	 ;; packages, then consider creating a layer. You can also put the
	 ;; configuration in `dotspacemacs/user-config'.
	 ;; To use a local version of a package, use the `:location' property:
	 ;; '(your-package :location "~/path/to/your-package/")
	 ;; Also include the dependencies as they will not be resolved automatically.
	 dotspacemacs-additional-packages '(
																			beacon
																			doom-themes
																			coffee-mode
																			)

	 ;; A list of packages that cannot be updated.
	 dotspacemacs-frozen-packages '()

	 ;; A list of packages that will not be installed and loaded.
	 dotspacemacs-excluded-packages '(
																		version-control
																		)

	 ;; Defines the behaviour of Spacemacs when installing packages.
	 ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
	 ;; `used-only' installs only explicitly used packages and deletes any unused
	 ;; packages as well as their unused dependencies. `used-but-keep-unused'
	 ;; installs only the used packages but won't delete unused ones. `all'
	 ;; installs *all* packages supported by Spacemacs and never uninstalls them.
	 ;; (default is `used-only')
	 dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
	"Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
	;; This setq-default sexp is an exhaustive list of all the supported
	;; spacemacs settings.
	(setq-default
	 ;; If non-nil then enable support for the portable dumper. You'll need
	 ;; to compile Emacs 27 from source following the instructions in file
	 ;; EXPERIMENTAL.org at to root of the git repository.
	 ;; (default nil)
	 dotspacemacs-enable-emacs-pdumper nil

	 ;; File path pointing to emacs 27.1 executable compiled with support
	 ;; for the portable dumper (this is currently the branch pdumper).
	 ;; (default "emacs-27.0.50")
	 dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

	 ;; Name of the Spacemacs dump file. This is the file will be created by the
	 ;; portable dumper in the cache directory under dumps sub-directory.
	 ;; To load it when starting Emacs add the parameter `--dump-file'
	 ;; when invoking Emacs 27.1 executable on the command line, for instance:
	 ;;		./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
	 ;; (default spacemacs.pdmp)
	 dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

	 ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
	 ;; possible. Set it to nil if you have no way to use HTTPS in your
	 ;; environment, otherwise it is strongly recommended to let it set to t.
	 ;; This variable has no effect if Emacs is launched with the parameter
	 ;; `--insecure' which forces the value of this variable to nil.
	 ;; (default t)
	 dotspacemacs-elpa-https t

	 ;; Maximum allowed time in seconds to contact an ELPA repository.
	 ;; (default 5)
	 dotspacemacs-elpa-timeout 30

	 ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
	 ;; This is an advanced option and should not be changed unless you suspect
	 ;; performance issues due to garbage collection operations.
	 ;; (default '(100000000 0.1))
	 dotspacemacs-gc-cons '(100000000 0.1)

	 ;; If non-nil then Spacelpa repository is the primary source to install
	 ;; a locked version of packages. If nil then Spacemacs will install the
	 ;; latest version of packages from MELPA. (default nil)
	 dotspacemacs-use-spacelpa nil

	 ;; If non-nil then verify the signature for downloaded Spacelpa archives.
	 ;; (default nil)
	 dotspacemacs-verify-spacelpa-archives nil

	 ;; If non-nil then spacemacs will check for updates at startup
	 ;; when the current branch is not `develop'. Note that checking for
	 ;; new versions works via git commands, thus it calls GitHub services
	 ;; whenever you start Emacs. (default nil)
	 dotspacemacs-check-for-update nil

	 ;; If non-nil, a form that evaluates to a package directory. For example, to
	 ;; use different package directories for different Emacs versions, set this
	 ;; to `emacs-version'. (default 'emacs-version)
	 dotspacemacs-elpa-subdirectory 'emacs-version

	 ;; One of `vim', `emacs' or `hybrid'.
	 ;; `hybrid' is like `vim' except that `insert state' is replaced by the
	 ;; `hybrid state' with `emacs' key bindings. The value can also be a list
	 ;; with `:variables' keyword (similar to layers). Check the editing styles
	 ;; section of the documentation for details on available variables.
	 ;; (default 'vim)
	 dotspacemacs-editing-style 'vim

	 ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
	 dotspacemacs-verbose-loading nil

	 ;; Specify the startup banner. Default value is `official', it displays
	 ;; the official spacemacs logo. An integer value is the index of text
	 ;; banner, `random' chooses a random text banner in `core/banners'
	 ;; directory. A string value must be a path to an image format supported
	 ;; by your Emacs build.
	 ;; If the value is nil then no banner is displayed. (default 'official)
	 dotspacemacs-startup-banner 'random

	 ;; List of items to show in startup buffer or an association list of
	 ;; the form `(list-type . list-size)`. If nil then it is disabled.
	 ;; Possible values for list-type are:
	 ;; `recents' `bookmarks' `projects' `agenda' `todos'.
	 ;; List sizes may be nil, in which case
	 ;; `spacemacs-buffer-startup-lists-length' takes effect.
	 dotspacemacs-startup-lists '((recents . 5)
																(projects . 7))

	 ;; True if the home buffer should respond to resize events. (default t)
	 dotspacemacs-startup-buffer-responsive t

	 ;; Default major mode of the scratch buffer (default `text-mode')
	 dotspacemacs-scratch-mode 'text-mode

	 ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
	 ;; (default nil)
	 dotspacemacs-initial-scratch-message nil

	 ;; List of themes, the first of the list is loaded when spacemacs starts.
	 ;; Press `SPC T n' to cycle to the next theme in the list (works great
	 ;; with 2 themes variants, one dark and one light)
	 dotspacemacs-themes '(
												 ;; doom-one
												 doom-opera
												 ;; sanityinc-tomorrow-bright
												 spacemacs-dark
												 )

	 ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
	 ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
	 ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
	 ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
	 ;; refer to the DOCUMENTATION.org for more info on how to create your own
	 ;; spaceline theme. Value can be a symbol or list with additional properties.
	 ;; (default '(spacemacs :separator wave :separator-scale 1.5))
	 dotspacemacs-mode-line-theme '(doom)

	 ;; If non-nil the cursor color matches the state color in GUI Emacs.
	 ;; (default t)
	 dotspacemacs-colorize-cursor-according-to-state nil

	 ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
	 ;; quickly tweak the mode-line size to make separators look not too crappy.
	 dotspacemacs-default-font '("Source Code Pro"
															 :size 12
															 :weight normal
															 :width normal)

	 ;; The leader key (default "SPC")
	 dotspacemacs-leader-key "SPC"

	 ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
	 ;; (default "SPC")
	 dotspacemacs-emacs-command-key "SPC"

	 ;; The key used for Vim Ex commands (default ":")
	 dotspacemacs-ex-command-key ":"

	 ;; The leader key accessible in `emacs state' and `insert state'
	 ;; (default "M-m")
	 dotspacemacs-emacs-leader-key "M-m"

	 ;; Major mode leader key is a shortcut key which is the equivalent of
	 ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
	 dotspacemacs-major-mode-leader-key ","

	 ;; Major mode leader key accessible in `emacs state' and `insert state'.
	 ;; (default "C-M-m")
	 dotspacemacs-major-mode-emacs-leader-key "C-M-m"

	 ;; These variables control whether separate commands are bound in the GUI to
	 ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
	 ;; Setting it to a non-nil value, allows for separate commands under `C-i'
	 ;; and TAB or `C-m' and `RET'.
	 ;; In the terminal, these pairs are generally indistinguishable, so this only
	 ;; works in the GUI. (default nil)
	 dotspacemacs-distinguish-gui-tab nil

	 ;; Name of the default layout (default "Default")
	 dotspacemacs-default-layout-name "Default"

	 ;; If non-nil the default layout name is displayed in the mode-line.
	 ;; (default nil)
	 dotspacemacs-display-default-layout t

	 ;; If non-nil then the last auto saved layouts are resumed automatically upon
	 ;; start. (default nil)
	 dotspacemacs-auto-resume-layouts nil

	 ;; If non-nil, auto-generate layout name when creating new layouts. Only has
	 ;; effect when using the "jump to layout by number" commands. (default nil)
	 dotspacemacs-auto-generate-layout-names nil

	 ;; Size (in MB) above which spacemacs will prompt to open the large file
	 ;; literally to avoid performance issues. Opening a file literally means that
	 ;; no major mode or minor modes are active. (default is 1)
	 dotspacemacs-large-file-size 1

	 ;; Location where to auto-save files. Possible values are `original' to
	 ;; auto-save the file in-place, `cache' to auto-save the file to another
	 ;; file stored in the cache directory and `nil' to disable auto-saving.
	 ;; (default 'cache)
	 dotspacemacs-auto-save-file-location 'cache

	 ;; Maximum number of rollback slots to keep in the cache. (default 5)
	 dotspacemacs-max-rollback-slots 5

	 ;; If non-nil, the paste transient-state is enabled. While enabled, after you
	 ;; paste something, pressing `C-j' and `C-k' several times cycles through the
	 ;; elements in the `kill-ring'. (default nil)
	 dotspacemacs-enable-paste-transient-state nil

	 ;; Which-key delay in seconds. The which-key buffer is the popup listing
	 ;; the commands bound to the current keystroke sequence. (default 0.4)
	 dotspacemacs-which-key-delay 0.4

	 ;; Which-key frame position. Possible values are `right', `bottom' and
	 ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
	 ;; right; if there is insufficient space it displays it at the bottom.
	 ;; (default 'bottom)
	 dotspacemacs-which-key-position 'bottom

	 ;; Control where `switch-to-buffer' displays the buffer. If nil,
	 ;; `switch-to-buffer' displays the buffer in the current window even if
	 ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
	 ;; displays the buffer in a same-purpose window even if the buffer can be
	 ;; displayed in the current window. (default nil)
	 dotspacemacs-switch-to-buffer-prefers-purpose nil

	 ;; If non-nil a progress bar is displayed when spacemacs is loading. This
	 ;; may increase the boot time on some systems and emacs builds, set it to
	 ;; nil to boost the loading time. (default t)
	 dotspacemacs-loading-progress-bar t

	 ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
	 ;; (Emacs 24.4+ only)
	 dotspacemacs-fullscreen-at-startup nil

	 ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
	 ;; Use to disable fullscreen animations in OSX. (default nil)
	 dotspacemacs-fullscreen-use-non-native nil

	 ;; If non-nil the frame is maximized when Emacs starts up.
	 ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
	 ;; (default nil) (Emacs 24.4+ only)
	 dotspacemacs-maximized-at-startup t

	 ;; A value from the range (0..100), in increasing opacity, which describes
	 ;; the transparency level of a frame when it's active or selected.
	 ;; Transparency can be toggled through `toggle-transparency'. (default 90)
	 dotspacemacs-active-transparency 100

	 ;; A value from the range (0..100), in increasing opacity, which describes
	 ;; the transparency level of a frame when it's inactive or deselected.
	 ;; Transparency can be toggled through `toggle-transparency'. (default 90)
	 dotspacemacs-inactive-transparency 80

	 ;; If non-nil show the titles of transient states. (default t)
	 dotspacemacs-show-transient-state-title t

	 ;; If non-nil show the color guide hint for transient state keys. (default t)
	 dotspacemacs-show-transient-state-color-guide t

	 ;; If non-nil unicode symbols are displayed in the mode line.
	 ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
	 ;; the value to quoted `display-graphic-p'. (default t)
	 dotspacemacs-mode-line-unicode-symbols t

	 ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
	 ;; scrolling overrides the default behavior of Emacs which recenters point
	 ;; when it reaches the top or bottom of the screen. (default t)
	 dotspacemacs-smooth-scrolling t

	 ;; Control line numbers activation.
	 ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
	 ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
	 ;; This variable can also be set to a property list for finer control:
	 ;; '(:relative nil
	 ;;		:disabled-for-modes dired-mode
	 ;;												doc-view-mode
	 ;;												markdown-mode
	 ;;												org-mode
	 ;;												pdf-view-mode
	 ;;												text-mode
	 ;;		:size-limit-kb 1000)
	 ;; (default nil)
	 dotspacemacs-line-numbers t

	 ;; Code folding method. Possible values are `evil' and `origami'.
	 ;; (default 'evil)
	 dotspacemacs-folding-method 'evil

	 ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
	 ;; (default nil)
	 dotspacemacs-smartparens-strict-mode nil

	 ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
	 ;; over any automatically added closing parenthesis, bracket, quote, etc…
	 ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
	 dotspacemacs-smart-closing-parenthesis nil

	 ;; Select a scope to highlight delimiters. Possible values are `any',
	 ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
	 ;; emphasis the current one). (default 'all)
	 dotspacemacs-highlight-delimiters 'all

	 ;; If non-nil, start an Emacs server if one is not already running.
	 ;; (default nil)
	 dotspacemacs-enable-server nil

	 ;; Set the emacs server socket location.
	 ;; If nil, uses whatever the Emacs default is, otherwise a directory path
	 ;; like \"~/.emacs.d/server\". It has no effect if
	 ;; `dotspacemacs-enable-server' is nil.
	 ;; (default nil)
	 dotspacemacs-server-socket-dir nil

	 ;; If non-nil, advise quit functions to keep server open when quitting.
	 ;; (default nil)
	 dotspacemacs-persistent-server nil

	 ;; List of search tool executable names. Spacemacs uses the first installed
	 ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
	 ;; (default '("rg" "ag" "pt" "ack" "grep"))
	 dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

	 ;; Format specification for setting the frame title.
	 ;; %a - the `abbreviated-file-name', or `buffer-name'
	 ;; %t - `projectile-project-name'
	 ;; %I - `invocation-name'
	 ;; %S - `system-name'
	 ;; %U - contents of $USER
	 ;; %b - buffer name
	 ;; %f - visited file name
	 ;; %F - frame name
	 ;; %s - process status
	 ;; %p - percent of buffer above top of window, or Top, Bot or All
	 ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
	 ;; %m - mode name
	 ;; %n - Narrow if appropriate
	 ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
	 ;; %Z - like %z, but including the end-of-line format
	 ;; (default "%I@%S")
	 dotspacemacs-frame-title-format "%I@%S"

	 ;; Format specification for setting the icon title format
	 ;; (default nil - same as frame-title-format)
	 dotspacemacs-icon-title-format nil

	 ;; Delete whitespace while saving buffer. Possible values are `all'
	 ;; to aggressively delete empty line and long sequences of whitespace,
	 ;; `trailing' to delete only the whitespace at end of lines, `changed' to
	 ;; delete only whitespace for changed lines or `nil' to disable cleanup.
	 ;; (default nil)
	 dotspacemacs-whitespace-cleanup 'trailing

	 ;; Either nil or a number of seconds. If non-nil zone out after the specified
	 ;; number of seconds. (default nil)
	 dotspacemacs-zone-out-when-idle nil

	 ;; Run `spacemacs/prettify-org-buffer' when
	 ;; visiting README.org files of Spacemacs.
	 ;; (default nil)
	 dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
	"Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
	(spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
	"Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

	;; allow emacs to find tern
	(add-to-list 'exec-path "/usr/local/bin")

	;; make inferior-tcl use tclsh (default is wish)
	(setq tcl-application "tclsh")

	;; Fix tramp, dammit
	(setq tramp-copy-size-limit 10240000)
	(setq tramp-inline-compress-start-size 4096000)
	(setq tramp-default-method "rsyncc")

	;; set up slime to use sbcl
	(load (expand-file-name "~/quicklisp/slime-helper.el"))
	(setq inferior-lisp-program "sbcl")

	;; (setq evil-want-integration t)
	;; (setq evil-want-keybinding nil)
	;; (with-eval-after-load 'eww
	;; 	(require 'evil-collection-eww)
	;; 	(evil-collection-eww-setup))

	)

(defun dotspacemacs/user-load ()
	"Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
	)

(defun dotspacemacs/user-config ()
	"Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

	;; fix savehist cpu hogging
	(setq history-length 100)
	(put 'minibuffer-history 'history-length 50)
	(put 'evil-ex-history 'history-length 50)
	(put 'kill-ring 'history-length 25)

	;; custom layer loading
	(add-to-list 'load-path "~/.emacs.d/layers/rest")
	(load "wttrin")
	(load "hackernews")
	(load "rivet-mode")
	(load "handlebars-mode")

	;; load private layers
	(load "~/.emacs.d/.private/privateConfig.el")

	;; set all dem tab sizes
	(setq-default indent-tabs-mode t)
	(setq-default tab-width 2)
	(setq-default css-indent-offset 2)
	(setq js-indent-level 2)
	(setq tcl-indent-level 2)
	(defvaralias 'c-basic-offset 'tab-width) ;; keep them the same
	(setq tab-stop-list (number-sequence 2 200 2))
	(setq tcl-tab-always-indent t)
	(setq standard-indent 2)
	(setq web-mode-markup-indent-offset 2)

	;; type to get rid of active selection
	(delete-selection-mode t)

	;; tabify on save
	;; (add-hook 'before-save-hook
	;;					(lambda ()
	;;						(tabify (point-min) (point-max))))

	;; fix up filename mappings
	(add-to-list 'auto-mode-alist '("\\.test\\'" . tcl-mode))
	(add-to-list 'auto-mode-alist '("\\.tpl\\'" . handlebars-mode))
	(add-to-list 'auto-mode-alist '("\\.rvt\\'" . rivet-mode))
	(add-to-list 'auto-mode-alist '("\\.css\\'" . scss-mode))
	(add-to-list 'auto-mode-alist '("\\Lakefile\\'" . common-lisp-mode))

	;; (require 'mmm-auto)
	;; (mmm-add-classes
	;;  '((html-rvt
	;; 		:submode tcl-mode
	;; 		:delimiter-mode nil
	;; 		:front "<\\?[=]?"
	;; 		:front-offset 1
	;; 		:back-offset 1
	;; 		:back "\\?>")))
	;; (setq mmm-submode-decoration-level 0)
	;; (setq mmm-global-mode 'maybe)
	;; (mmm-add-mode-ext-class 'html-mode "\\.rvt\\'" 'html-rvt)
	;; (setq auto-mode-alist (append (list (cons "\\.rvt\\'" 'html-mode))
	;; 															auto-mode-alist))

	;; AGGRESSIVELY INDENT THAT SHIT
	(add-hook 'tcl-mode #'aggressive-indent-mode)
	(add-hook 'scss-mode #'aggressive-indent-mode)
	(add-hook 'emacs-lisp-mode #'aggressive-indent-mode)
	(add-hook 'common-lisp-mode #'aggressive-indent-mode)

	;; eshell
	(setq eshell-aliases-file (concat user-emacs-directory "eshell-aliases"))
	(setq eshell-save-history-on-exit t
				eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")
	(setq eshell-prompt-function
				(lambda ()
					(concat
					 "\n"
					 (propertize " ┌─── " 'face '(:foreground "green"))
					 (propertize (eshell/pwd) 'face '(:weight ultra-bold))
					 "\n"
					 (propertize " └─ λ " 'face '(:foreground "green")))))

	;; apologize for smacking emacs (stop debug on quit)
	;; use this after smacking emacs with SIGUSR2
	(defun apologize-to-emacs ()
		(interactive)
		(toggle-debug-on-quit))

	;; make tramp play nicely with hopnu (and others)
	(setq shell-file-name "/usr/local/bin/bash")

	;; cache that project!
	(setq projectile-indexing-method 'alien)
	(setq projectile-enable-caching t)

	;; I really want tabs all the time always
	(setq indent-tabs-mode t)

	;; enable beacon
	(beacon-mode 1)

	;; doom modeline
	(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

	;; neotree theme
	(setq neo-theme (if (display-graphic-p) 'icons 'nerd))

	;; fix magit colors and things
	(eval-after-load 'magit
		'(progn
			 (set-face-attribute 'magit-diff-added nil :foreground "green")
			 (set-face-attribute 'magit-diff-added-highlight nil :foreground "green" :background "color-234" :weight 'bold)
			 (set-face-attribute 'magit-diff-context-highlight nil :weight 'bold :foreground "white" :background "color-234")
			 (set-face-attribute 'magit-diff-hunk-heading nil :weight 'bold :background "blue")
			 (set-face-attribute 'magit-diff-hunk-heading-highlight nil :weight 'bold :background "blue")
			 (set-face-attribute 'magit-diff-hunk-region nil :weight 'bold :background "black")
			 (set-face-attribute 'magit-diff-lines-heading nil :foreground "white" :background "blue")
			 (set-face-attribute 'magit-diff-removed nil :foreground "red" :background "color-232")
			 (set-face-attribute 'magit-diff-removed-highlight nil :weight 'bold :foreground "red" :background "color-234")
			 ))
	(setq magit-diff-paint-whitespace t)
	(setq magit-diff-highlight-trailing t)
	(set-face-attribute 'hl-line nil :background nil)
	(set-face-attribute 'cursor nil :background "white")
	(set-face-attribute 'default nil :background nil :foreground "white")

	;; word wrap in all text mode, including eww
	(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

	;; osx thangs
	(defun osx-get-from-clipboard ()
		"Get clipboard text"
		(shell-command-to-string "pbpaste"))
	(defun osx-paste-from-clipboard ()
		"Paste from clipboard"
		(interactive)
		(insert (osx-get-from-clipboard)))
	(defun osx-copy-to-clipboard (&optional text)
		"Copy the given text to clipboard"
		(interactive)
		(unless text
			(setq text (buffer-substring (mark) (point))))
		(shell-command-to-string (concat "pbcopy < <(echo -n " (shell-quote-argument text) ")")))

	;; setup eww
	(setq browse-url-browser-function 'eww-browse-url)
	;; (setq browse-url-browser-function 'browse-url-default-macosx-browser)
	(defun add-title-to-eww-buffer-name ()
		"Rename eww mode buffer so the title of the page is displayed, making
     fake-tabbed-browsing easier"
		(let ((title (plist-get eww-data :title)))
			(when (eq major-mode 'eww-mode)
				(if title
						(rename-buffer (concat "eww - " title) t)
					(rename-buffer "eww" t)))))
	(add-hook 'eww-after-render-hook 'add-title-to-eww-buffer-name)

	;; sql comint
	(add-to-list 'same-window-buffer-names "*SQL: *")

	;; mouse scroll
	(defun up-slightly () (interactive) (scroll-up 5))
	(defun down-slightly () (interactive) (scroll-down 5))
	(global-set-key [mouse-4] 'down-slightly)
	(global-set-key [mouse-5] 'up-slightly)

	;; tcl flycheck
	(load "tclchecker")
	(with-eval-after-load 'flycheck
		(flycheck-tcl-setup))
	(defun flyspell-ignore-http-and-https ()
		"Function used for `flyspell-generic-check-word-predicate' to ignore stuff
		starting with \"http\" or \"https\"."
		(save-excursion
			(forward-whitespace -1)
			(when (looking-at " ")
        (forward-char)
				(not (looking-at "https?\\b")))))
	(put 'text-mode 'flyspell-mode-predicate 'flyspell-ignore-http-and-https)

	(defun thornlog ()
		"Quickly open my daily professional log (journal)"
		(interactive)
		(find-file "~/doc/thornlog.org"))

)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flyspell-correct-helm flyspell-correct auto-dictionary writeroom-mode visual-fill-column yasnippet-snippets yaml-mode xterm-color xkcd ws-butler winum which-key web-mode web-beautify volatile-highlights vimrc-mode vi-tilde-fringe uuidgen use-package unfill toc-org tagedit symon string-inflection sql-indent spaceline-all-the-icons smeargle slime-company slim-mode shell-pop scss-mode sass-mode restart-emacs rainbow-delimiters pug-mode prettier-js popwin pomidor persp-mode pcre2el password-generator paradox overseer orgit org-projectile org-present org-pomodoro org-mime org-download org-bullets org-brain open-junk-file neotree nameless mwim multi-term move-text mmm-mode markdown-toc magit-svn magit-gitflow lorem-ipsum livid-mode link-hint json-navigator json-mode js2-refactor js-doc indent-guide impatient-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-purpose helm-projectile helm-org-rifle helm-mode-manager helm-make helm-gtags helm-gitignore helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag handlebars-mode google-translate golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md ggtags fuzzy font-lock+ flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-collection evil-cleverparens evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav editorconfig dumb-jump dotenv-mode doom-themes doom-modeline diminish define-word dactyl-mode counsel-projectile company-web company-tern company-statistics common-lisp-snippets column-enforce-mode coffee-mode clean-aindent-mode centered-cursor-mode beacon auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
