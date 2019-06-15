;; -*- lexical-binding: t -*-
;;
;; Aero core prelude layer
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs


;; garder ma merde à jour

(use-package auto-package-update :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  (auto-package-update-maybe))


;; the general is here

(use-package which-key :ensure t)
(use-package general :ensure t
	:config
	(general-define-key
	 :states '(normal motion)
	 :prefix "SPC"
	 "" nil))


;; we descend to hell

(use-package evil :ensure t
  :commands (evil-mode)
  :init
  (aero/add-hook!
   'after-init-hook
   (evil-mode 1)
   (setq evil-want-fin-undo t))
	(general-define-key
	 :states 'normal
	 :prefix "SPC"
	 "fS" 'evil-write-all))

(use-package evil-matchit :ensure t
  :config
  (global-evil-matchit-mode 1))

(use-package evil-surround :ensure t
  :config
  (global-evil-surround-mode))

(use-package evil-visualstar :ensure t
  :config
  (global-evil-visualstar-mode t))

(setq evil-default-state 'normal)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'message-mode 'motion)
(evil-set-initial-state 'help-mode 'emacs)
(evil-set-initial-state 'ivy-occur-mode 'emacs)
(evil-set-initial-state 'calendar-mode 'emacs)
(evil-set-initial-state 'esup-mode 'emacs)

;; cursor color by state
(setq evil-insert-state-cursor  '("#268bd2" hbar)  ;; blue
      evil-normal-state-cursor  '("#b58900" box)  ;; blue
      evil-visual-state-cursor  '("#cb4b16" bar)  ;; orange
      evil-replace-state-cursor '("#859900" hbar) ;; green
      evil-emacs-state-cursor   '("#d33682" box)) ;; magenta


;; abo-abo!

(use-package counsel :ensure t
  :config
  (setq counsel-find-file-ignore-regexp
        (concat "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)"
                "\\|\\.x\\'\\|\\.d\\'\\|\\.o\\'"
                "\\|\\.aux\\'"))
	(general-define-key
	 :states '(normal visual insert replace emacs)
	 :prefix "SPC"

	 "SPC" 'counsel-M-x))

(use-package ivy :ensure t
	:config
	(setq ivy-initial-inputs-alist nil))


;; keybindings

(general-define-key
 :states '(normal visual insert replace emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"

 ;; independent keys
 "TAB" '((switch-to-buffer (other-buffer (current-buffer) 1))
				 :which-key "last buffer")

 "f" '(:ignore t :which-key "files")
 "fw" '(save-buffer :which-key "write buffer")
 "fC" '(:ignore t :which-key "convert")
 "fCd" '(aero/unix2dos :which-key "unix2dos")
 "fCu" '(aero/dos2unix :which-key "dos2unix")
 "fD" '(aero/delete-this-file :which-key "delete this file")
 "fE" '(aero/sudo-edit :which-key "sudo edit")
 "fR" '(aero/rename-this-file-and-buffer :which-key "rename this file"))

(provide 'aero-prelude)
