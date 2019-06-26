;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs 27+ introduces early-init.el, which is run before init.el, before
;; package and UI initialization happens.

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We want Aero to handle this, so stop
;; Emacs
(setq package-enable-at-startup nil)

;; Turn off mouse interface and other styles
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; One less file to load at startup
(setq site-run-file nil)
