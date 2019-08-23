;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
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

(use-package tcl
  :mode "\\.\\(tcl|test\\)\\'"

  :init
	;; make inferior-tcl use tclsh (default is wish)
	(setq tcl-application "tclsh"))

(use-package testbackend ; local
  :general
  (:states 'normal
   :keymaps 'tcl-mode-map
   :prefix "SPC"
   ",t" '(:ignore t :which-key "testbackend")
   ",tr" 'testbackend/run-tests
   ",tR" 'testbackend/run-tests-no-cache
   ",tw" 'testbackend/run-tests-on-write
   ",tS" 'testbackend/stop-run-tests-on-write
   ",tt" 'testbackend/set-test-target
   ",tT" 'testbackend/clear-test-target))

(use-package rivet-mode ; local
  :mode "\\.rvt\\'")

(provide 'aero-tcl)
