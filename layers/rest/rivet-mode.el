;; rivet-mode.el -- A major mode for Apache Rivet
;;
;; Author: Jade Michael Thornton
;; Copyright 2019
;; Version: 2.0.0
;; Package-Requires ((emacs "25") (polymode "0.1.5") (web-mode) (tcl-mode))
;; URL: https://gitlab.com/thornjad/rivet
;;
;; This file is not part of GNU Emacs
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;
;; Commentary;
;;
;; This package defines rivet-mode, a major mode for Apache Rivet which
;; leverages the power of polymode.

(require 'polymode)

(define-hostmode rivet-hostmode
	:mode 'web-mode)

(define-innermode rivet-innermode
	:mode 'tcl-mode
	:head-matcher "<\\?=?"
	:tail-matcher "\\?>"
	:head-mode 'host
	:tail-mode 'host)

;;;###autoload (autoload 'rivet-mode "rivet-mode")
(define-polymode rivet-mode
	:hostmode 'rivet-hostmode
	:innermodes '(rivet-innermode))

(add-to-list 'auto-mode-alist '("\\.rvt" . rivet-mode))

(provide 'rivet-mode)

;;; rivet-mode.el ends here
