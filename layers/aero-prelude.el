;; -*- lexical-binding: t -*-
;;
;; Aero core prelude layer
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the therms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs


;; garder ma merde à jour

(use-package auto-package-update
	:ensure t
	:config
	(setq auto-package-update-delete-old-versions t
				auto-package-update-hide-results t)
	(auto-package-update-maybe))


;; ido-ido

(use-package counsel :ensure t
	:config
	(setq counsel-find-file-ignore-regexp
				(concat "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)"
								"\\|\\.x\\'\\|\\.d\\'\\|\\.o\\'"
								"\\|\\.aux\\'")))


;; Set up global functionality

(use-package which-key :ensure t)
(use-package general :ensure t)
(use-package diminish :ensure t)


;; keybindings

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"

 ;; simple commands
 "TAB" '(switch-to-other-buffer :which-key "prev buffer")
 "SPC" '(counsel-M-x :which-key "M-x")
 "'" '(eshell :which-key "eshell"))

(provide 'aero-prelude)
