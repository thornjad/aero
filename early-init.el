;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs 27+ introduces early-init.el, which is run before init.el, before
;; package and UI initialization happens.

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We want Aero to handle this, so stop
;; Emacs
(setq package-enable-at-startup nil)

;; remove useless crap
(byte-code
 "\301\302!\203\n \302\303!\210\301\304!\203 \304\303!\210\301\305!\203 \305\303!\210\301\306!\203( \306\307!\210\307\310\311\312\"\210\310\311\313\"\210\310\311\314\"\207"
 [fringe-mode fboundp menu-bar-mode -1 tool-bar-mode scroll-bar-mode
              set-fringe-mode 0 add-to-list default-frame-alist (tool-bar-lines . 0)
              (menu-bar-lines . 0) (vertical-scroll-bars)] 3)

;; One less file to load at startup, and we'll never use it
(setq site-run-file nil)
