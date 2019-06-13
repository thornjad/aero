;; -*- lexical-binding: t -*-
;;
;; Aero core prelude layer
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
;;
;; This program is free software; permission to use, copy, modify, and/or
;; distribute this software for any purpose with or without fee is hereby
;; granted, provided that the above copyright notice and this permission notice
;; appear in all copies.
;;
;; The software is provided "as is" and the author disclaims all warranties with
;; regard to this software including all implied warranties of merchantability
;; and fitness. In no event shall the author be liable for any special, direct,
;; indirect, or consequential damages or any damages whatsoever resulting from
;; loss of use, data or profits, whether in an action of contract, Negligence or
;; other tortious action, arising out of or in connection with the use or
;; performance of this software.
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
