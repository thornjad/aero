;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs 27+ introduces early-init.el, which is run before init.el, before
;; package and UI initialization happens.

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We want Aero to handle this, so stop
;; Emacs
(setq package-enable-at-startup nil)

;; Désactiver les menus, les franges, etc. Nous utilisons du
;; byte-code car il fallait 45 ms pour s’exécuter.
(byte-code
 "\300\301!\203\n \301\302!\210\300\303!\203 \303\302!\210\300\304!\203 \304\302!\210\305\306\307\"\210\305\306\310\"\210\305\306\311\"\210\312\313!\207"
 [fboundp menu-bar-mode -1 tool-bar-mode scroll-bar-mode add-to-list
          default-frame-alist (tool-bar-lines . 0) (menu-bar-lines . 0)
          (vertical-scroll-bars) set-fringe-mode 0] 3)

;; One less file to load at startup, and we'll never use it
(setq site-run-file nil)
