;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2020 Jade Michael Thornton
;;
;; This file is not part of GNU Emacs
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; The software is provided "as is" and the author disclaims all warranties with
;; regard to this software including all implied warranties of merchantability
;; and fitness. In no event shall the author be liable for any special, direct,
;; indirect, or consequential damages or any damages whatsoever resulting from
;; loss of use, data or profits, whether in an action of contract, negligence or
;; other tortious action, arising out of or in connection with the use or
;; performance of this software.
;;
;;; Commentary:
;;
;;; Code:

(require 'aero-modeline)
(aero/modeline-global-mode +1)

;; in etc/themes/
(load-theme 'aero-light t)

;; Other themes to browse from time to time
;; (use-package doom-themes :straight t
;;   :defer 10
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t)
;;   (doom-themes-neotree-config)
;;   (doom-themes-org-config))
;; (use-package poet-theme :straight t :defer 10)
;; (use-package spacemacs-theme :straight t :defer 10)
;; (use-package spaceline :straight t)
(use-package tao-theme :straight t :defer 10
  :config
  (setq tao-theme-use-boxes nil
        tao-theme-use-height nil))

(setq default-frame-alist
      (append (list '(width  . 120) '(height . 45)
                    '(tool-bar-lines . 0)
                    '(menu-bar-lines . 1)
                    '(internal-border-width . 10)
                    '(left-fringe . 0) '(right-fringe . 0)
                    '(vertical-scroll-bars . nil)
                    '(ns-transparent-titlebar . t)
                    '(ns-appearance . dark))))
(set-frame-parameter (selected-frame)
                     'internal-border-width 10)


;;; weather and date in echo area

(require 'cl-lib)
(require 'request)

(defvar aero/weather-location "KDWH"
  "Location to display current weather for.")
(defvar aero/weather-update-interval "30 min"
  "The time interval at which to update the weather.")
(defvar aero/current-weather ""
  "Current weather at `aero/weather-location'.")
(defvar aero--current-weather-update-timer ""
  "Timer for updating the current weather.")

(defun aero--update-weather-line ()
  ;; TODO include sunrise/set if somewhat near that time
  "Set the current weather into `aero/current-location'."
  ;; Ensure enhanced-message is active
  (aero/activate-enhanced-message)
  (request
    (concat "http://wttr.in/" aero/weather-location)
    ;; format is detailed at https://github.com/chubin/wttr.in#one-line-output
    :params '(("format" . "%t %C"))
    :success (cl-function (lambda (&key data &allow-other-keys)
                            (setq aero/current-weather data))))

  ;; Reset the auto-update timer
  (when (timerp aero--current-weather-update-timer)
    (cancel-timer aero--current-weather-update-timer))
  (setq aero--current-weather-update-timer
        (run-at-time aero/weather-update-interval nil
                     #'aero--update-weather-line)))

(defun aero/update-weather ()
  "Force update the weather."
  (interactive)
  (aero--update-weather-line))

(defun aero/stop-weather ()
  "Stop the weather."
  (interactive)
  (when (timerp aero--current-weather-update-timer)
    (cancel-timer aero--current-weather-update-timer))
  (setq aero/current-weather ""))

;; Improved version of enhanced-message.el
;; https://gist.github.com/rougier/baaf4ff6e0461680e3f070c5c32b64a2
(defun enhanced-message (orig-fun &rest args)
  "Message ORIG-FUN with ARGS, but add additional text.
This enhanced message displays a regular message in the echo area and adds a
specific text on the right part of the echo area. This is to be used as an
advice."
  (when-let* ((right (propertize
                      ;; HACK The first and last spaces are thin spaces, not
                      ;; regular spaces. We'll split on the thin space later
                      ;; so-as not to inadvertently break up a real message
                      (concat
                       "   "
                       (or aero/current-weather "")
                       "   "
                       (format-time-string " %A %d %B %Y, %H:%M")
                       "   ")
                      'face '(:height 0.85
                              :overline t
                              :family "Futura"
                              :inherit mode-line-inactive)))
              (width (- (frame-width) (length right) -5))
              (msg (if (car args) (apply 'format-message args) ""))
              (msg (car (split-string msg " ")))
              (msg (string-trim msg))
              (left (truncate-string-to-width msg width nil nil "…"))
              (full (format (format "%%-%ds %%s" width) left right)))
    (if (active-minibuffer-window)
        ;; Regular log and display when minibuffer is active
        (apply orig-fun args)
      (progn
        ;; Log actual message without echo
        (when message-log-max
          (let ((inhibit-message t))
            (apply orig-fun (list msg))))
        ;; Display enhanced message without log. Replace any straggling
        ;; format-like strings from the original message so Emacs doesn't get
        ;; confused.
        (let ((message-truncate-lines t) (message-log-max nil)
              (full (replace-regexp-in-string "%" "%%" full)))
          (apply orig-fun (list full)))
        ;; Set current message explicitly
        (setq current-message msg)))))

(defun aero/show-enhanced-message ()
  (let ((message-log-max nil))
    (message (current-message))))

(defun aero/activate-enhanced-message ()
  "Ensure `enhanced-message' is active."
  (unless (advice-member-p #'message #'enhanced-message)
    (advice-add 'message :around #'enhanced-message))
  (unless (memq #'aero/show-enhanced-message post-command-hook)
    (add-hook 'post-command-hook #'aero/show-enhanced-message)))

;; Initialize enhanced-message
(aero--update-weather-line)
(aero/activate-enhanced-message)


;;; get ligatures to actually work

(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; Better fringe wrap symbols. Victor Mono doesn't provide these symbols, so use
;; Fira Code
(fringe-mode '(0 . 0))
(defface fallback
  '((t :family "Fira Code Light"
       :inherit 'default
       :group 'faces))
  "Fallback font")
;; TODO these aren't working, they say "wrong-type-argument char-table-p nil" and show that standard-display-table is nil? Do we need to wait for somethign to eval?
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'fallback))
(set-display-table-slot standard-display-table 'selective-display
                        (string-to-vector " …"))


;;; additional tweaks and packages

(blink-cursor-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(pixel-scroll-mode 1)
(global-display-fill-column-indicator-mode 1)
(add-hook 'text-mode-hook #'turn-on-visual-line-mode)
(add-hook 'fundamental-mode-hook #'turn-on-visual-line-mode)

(use-package all-the-icons :straight t)

(use-package formfeeder
  :load-path "lib/packages/formfeeder/"
  :hook (text-mode . formfeeder-mode)
  :config
  (setq formfeeder-line-width 80)
  (declare-function global-formfeeder-mode "formfeeder")
  (global-formfeeder-mode 1))

(use-package todo-light
  :load-path "lib/packages/todo-light/"
  :hook ((prog-mode text-mode) . todo-light-mode))

(use-package fireplace :straight t
  :commands fireplace)

(defun pulse-line (&rest _)
  "Briefly pulse a highlight of the line at point.
This function, when bound to certain commands like scrolling, acts as a native
alternative to the beacon package."
  (pulse-momentary-highlight-one-line (point)))
(dolist (cmd '(scroll-up-command
               scroll-down-command
               recenter-top-bottom
               other-window))
  (advice-add cmd :after #'pulse-line))
(when (require 'evil nil 'no-error)
  (dolist (cmd '(evil-goto-first-line
                 evil-goto-line
                 evil-scroll-up
                 evil-scroll-down
                 evil-scroll-line-to-center
                 evil-scroll-line-to-top
                 evil-scroll-line-to-bottom
                 evil-window-top
                 evil-window-middle
                 evil-window-bottom))
    (advice-add cmd :after #'pulse-line)))

(provide 'aero-theme)
