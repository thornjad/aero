;;; effective-language.el --- Mark weak language in Emacs

;; Copyright (c) 2020-2023 Jade Michael Thornton

;; Filename: effective-language.el
;; Description: A minor mode for Emacs that highlights weak language
;; Version: 0.9
;; Keywords: English, grammar, E-prime, weasel

;; Based in part on eprime-mode.el, https:gitlab.com/thornjad/eprime-mode

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

;; This file is not part of GNU Emacs

;;; Commentary:

;; A minor mode for Emacs which marks weak language in Emacs.

;;; Code:

(defcustom effective-language-ignore-case t
  "Whether `effective-language-mode' should ignore case.")

(defcustom effective-language-eprime t
  "Whether `effective-language-mode' marks words banned from E-prime.")

(defcustom effective-language-eprime-words
  '("be" "being" "been" "am" "is" "isn't" "are" "aren't" "was" "wasn't" "were" "weren't" "I'm" "i'm"
    "you're" "we're" "they're" "he's" "she's" "it's" "there's" "here's" "where's" "how's" "what's"
    "who's" "what's" "ain't" "whatcha" "yer")
  "Banned words according to E-prime.")

(defcustom effective-language-no-just t
  "Whether `effective-language-mode' marks the word \"just\" for review.")

(defcustom effective-language-weasel t
  "Whether `effective-language-mode' marks weasel words.")

(defcustom effective-language-weasel-words
  '("many" "various" "very" "fairly" "several" "extremely" "exceedingly" "quite" "remarkably" "few"
    "surprisingly" "probably" "possibly" "mostly" "largely" "huge" "tiny" "are a number" "is a
    number" "excellent" "basically" "honestly" "interestingly" "substantially" "I would say"
    "clearly" "vast" "relatively" "literally" "not rocket science" "outside the box" "research
    shows")
  "Words which count as weasel words.")

(defcustom effective-language-passive-voice t
  "Whether `effective-language-mode' marks passive voice.")

(defcustom effective-language-passive-voice-irregulars
  '("awoken" "been" "born" "beat" "become" "begun" "bent" "beset" "bet" "bid" "bidden" "bound"
    "bitten" "bled" "blown" "broken" "bred" "brought" "broadcast" "built" "burnt" "burst" "bought"
    "cast" "caught" "chosen" "clung" "come" "cost" "crept" "cut" "dealt" "dug" "dived" "done"
    "drawn" "dreamt" "driven" "drunk" "eaten" "fallen" "fed" "felt" "fought" "found" "fit" "fled"
    "flung" "flown" "forbidden" "forgotten" "foregone" "forgiven" "forsaken" "frozen" "gotten"
    "given" "gone" "ground" "grown" "hung" "heard" "hidden" "hit" "held" "hurt" "kept" "knelt"
    "knit" "known" "laid" "led" "leapt" "learnt" "left" "lent" "let" "lain" "lighted" "lost" "made"
    "meant" "met" "misspelt" "mistaken" "mown" "overcome" "overdone" "overtaken" "overthrown" "paid"
    "pled" "proven" "put" "quit" "read" "rid" "ridden" "rung" "risen" "run" "sawn" "said" "seen"
    "sought" "sold" "sent" "set" "sewn" "shaken" "shaven" "shorn" "shed" "shone" "shod" "shot"
    "shown" "shrunk" "shut" "sung" "sunk" "sat" "slept" "slain" "slid" "slung" "slit" "smitten"
    "sown" "spoken" "sped" "spent" "spilt" "spun" "spit" "split" "spread" "sprung" "stood" "stolen"
    "stuck" "stung" "stunk" "stridden" "struck" "strung" "striven" "sworn" "swept" "swollen" "swum"
    "swung" "taken" "taught" "torn" "told" "thought" "thrived" "thrown" "thrust" "trodden"
    "understood" "upheld" "upset" "woken" "worn" "woven" "wed" "wept" "wound" "won" "withheld"
    "withstood" "wrung" "written")
  "Words which are passive irregulars.")

(defvar effective-language-duplicates-font-lock-keywords-regexp
  "\\b\\([[:word:]]+\\)\\([[:space:]]\\|\\s<\\|\\s>\\)+\\1\\b"
  "Font-lock keywords for duplicates")

(provide 'effective-language)
