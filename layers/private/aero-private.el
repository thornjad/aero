;; -*- lexical-binding: t -*-
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
;;
;;; Commentary:
;;
;; This directory is private to each instance of Aero, and it will
;; never be committed, with the notable exception of this file. Use
;; this directory to store sensitive information and settings, such as
;; database connections, passwords, etc.
;;
;; Each file in this directory /should/ properly provide a package
;; beginning with the prefix "aero-private-". However, this is not
;; enforced.
;;
;; This file provides only one function, `aero/load-private', which
;; loads an uncommitted file, aero-private-hidden.el, if it
;; exists. That other file can be used in any way, including loading
;; other hidden packages in this directory.
;;
;;; Code:

(defun aero/load-private ()
  "Load the hidden private package."
  (require 'aero-private-hidden nil t))

(provide 'aero-private)
