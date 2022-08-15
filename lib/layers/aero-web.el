;; -*- lexical-binding: t -*-
;;
;; web, js, cs, etc.
;;
;; Copyright (c) 2018-2022 Jade Michael Thornton
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
;;; Code:

(require 'aero-lib)
(require 'aero-prelude)

(use-package yarn :straight (:host github :repo "jmfirth/yarn.el")
  :commands (yarn-clean yarn-info yarn-init yarn-install yarn-add yarn-link yarn-run yarn-remove
                        yarn-update yarn-self-update yarn-test yarn-unlink yarn-why))
(use-package npm :straight t :commands (npm))

(defun aero/jest-file ()
  "Run jest on the file in this buffer."
  (interactive)
  (let ((file (buffer-file-name))
        (default-directory (project-root (project-current))))
    (compile (concat "npx jest " file))))
(defun aero/jest-file-watch ()
  "Run jest on the file in this buffer and watch.

Requires watchman."
  (interactive)
  (let ((file (buffer-file-name))
        (default-directory (project-root (project-current))))
    (compile (concat "npx jest --watch " file))))
(defun aero/jest ()
  "Run jest in this project."
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (compile "npx jest")))

(use-package jest
  ;; jest-traversal is required for for some reason doesn't come through in straight
  :straight (:host github :repo "emiller88/emacs-jest" :files ("jest.el" "jest-traversal.el"))
  :after general
  :commands (jest jest-file jest-file-dwim jest-function jest-last-failed jest-repeat)
  :init
  (aero-mode-leader-def
    :keymaps '(js2-mode web-mode typescript-mode)
    "j" '(:ignore t :wk "jest")
    "jf" 'aero/jest-file
    "jF" 'aero/jest-file-watch
    "j RET" 'aero/jest
    "jd" '(jest-function :wk "jest defun")
    "jr" 'jest-repeat
    "jl" 'jest-last-failed))

(use-package web-mode :straight t
  :mode "\\.\\(jsp\\|tpl\\|php\\|xml\\|html?\\|svg\\|jsx\\|tsx\\)\\'"
  :preface
  ;; NOTE: Not automatic, load via dir-locals whenever web-mode loads:
  ;;     ((web-mode (eval web-angular-mode)))
  (define-derived-mode web-angular-mode web-mode "Web/Angular"
    "Helper mode for Angular .component.html files.")
  :config
  (setq web-mode-engines-alist
        '(("ctemplate" . "\\.tpl\\'"))))

(use-package emmet-mode :straight t
  :load-path "lib/packages/emmet-mode/"
  :hook ((web-mode html-mode css-mode scss-mode js-mode) . emmet-mode)
	:init
	(setq emmet-self-closing-tag-style " /")

  :config
  (add-hook
   'js-mode-hook
   (lambda () (setq emmet-expand-jsx-className? t))))

(use-package scss-mode :straight t
  :mode "\\.s?css\\'")


;; js and jsx

(defun node-repl ()
  "Launch a Node.js comint REPL."
  (interactive)
  (setenv "NODE_NO_READLINE" "1")  ; avoid fancy terminal codes
  (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive")))

(eval-when-compile (defvar emmet-expand-jsx-className?))
(add-hook 'js-mode-hook (lambda () (setq emmet-expand-jsx-className? t)))

(use-package json-mode :straight t
	:mode "\\.json\\'")

(use-package typescript-mode :straight t :mode "\\.tsx?\\'")

;; Seems inferior to web-mode and ts-mode?
;; (use-package ng2-mode :straight t)


;; the rest

(use-package restclient
	:after (general)
  :commands (restclient-mode)
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (require 'general)
  (aero-mode-leader-def
    :keymaps 'restclient-mode-map
    "RET" '(restclient-http-send-current-stay-in-window :wk "Run query at point")
    "c" '(restclient-http-send-current :wk "Run query at point and focus")
    "r" '(restclient-http-send-current-raw :wk "Run query, no pretty print")
    "n" 'restclient-jump-next
    "p" 'restclient-jump-prev
    "." 'restclient-mark-current
    "y" 'restclient-copy-curl-command))

(use-package company-restclient :straight t
  :hook (restclient-mode-hook . company-restclient)
  :after (restclient company)
  :config
  (add-to-list 'company-backends 'company-restclient))


;; HTML entity helper


(defvar aero/html-entity-list
  '("char: \" entity: &quot;      text: quotation mark (APL quote)"
    "char: & entity: &amp;       text: ampersand"
    "char: ' entity: &apos;      text: apostrophe (apostrophe-quote)"
    "char: < entity: &lt;        text: less-than sign"
    "char: > entity: &gt;        text: greater-than sign"
    "char:   entity: &nbsp;      text: no-break space (non-breaking space)"
    "char: ¡ entity: &iexcl;     text: inverted exclamation mark"
    "char: ¢ entity: &cent;      text: cent sign"
    "char: £ entity: &pound;     text: pound sign"
    "char: ¤ entity: &curren;    text: currency sign"
    "char: ¥ entity: &yen;       text: yen sign (yuan sign)"
    "char: ¦ entity: &brvbar;    text: broken bar (broken vertical bar)"
    "char: | entity: &#124;      text: vertical bar (unix pipe)"
    "char: § entity: &sect;      text: section sign"
    "char: ¨ entity: &uml;       text: diaeresis (spacing diaeresis); see Germanic umlaut"
    "char: © entity: &copy;      text: copyright symbol"
    "char: ª entity: &ordf;      text: feminine ordinal indicator"
    "char: « entity: &laquo;     text: left-pointing double angle quotation mark (left pointing guillemet)"
    "char: ¬ entity: &not;       text: not sign"
    "char:   entity: &shy;       text: soft hyphen (discretionary hyphen)"
    "char: ® entity: &reg;       text: registered sign (registered trademark symbol)"
    "char: ¯ entity: &macr;      text: macron (spacing macron, overline, APL overbar)"
    "char: ° entity: &deg;       text: degree symbol"
    "char: ± entity: &plusmn;    text: plus-minus sign (plus-or-minus sign)"
    "char: ² entity: &sup2;      text: superscript two (superscript digit two, squared)"
    "char: ³ entity: &sup3;      text: superscript three (superscript digit three, cubed)"
    "char: ´ entity: &acute;     text: acute accent (spacing acute)"
    "char: µ entity: &micro;     text: micro sign"
    "char: ¶ entity: &para;      text: pilcrow sign (paragraph sign)"
    "char: · entity: &middot;    text: middle dot (Georgian comma, Greek middle dot)"
    "char: ¸ entity: &cedil;     text: cedilla (spacing cedilla)"
    "char: ¹ entity: &sup1;      text: superscript one (superscript digit one)"
    "char: º entity: &ordm;      text: masculine ordinal indicator"
    "char: » entity: &raquo;     text: right-pointing double angle quotation mark (right pointing guillemet)"
    "char: ¼ entity: &frac14;    text: vulgar fraction one quarter (fraction one quarter)"
    "char: ½ entity: &frac12;    text: vulgar fraction one half (fraction one half)"
    "char: ¾ entity: &frac34;    text: vulgar fraction three quarters (fraction three quarters)"
    "char: ¿ entity: &iquest;    text: inverted question mark (turned question mark)"
    "char: ` entity: &#96;       text: grave accent"
    "char: À entity: &Agrave;    text: Latin capital letter A with grave accent (Latin capital letter A grave)"
    "char: Á entity: &Aacute;    text: Latin capital letter A with acute accent"
    "char: Â entity: &Acirc;     text: Latin capital letter A with circumflex"
    "char: Ã entity: &Atilde;    text: Latin capital letter A with tilde"
    "char: Ä entity: &Auml;      text: Latin capital letter A with diaeresis"
    "char: Å entity: &Aring;     text: Latin capital letter A with ring above (Latin capital letter A ring)"
    "char: Æ entity: &AElig;     text: Latin capital letter AE (Latin capital ligature AE)"
    "char: Ç entity: &Ccedil;    text: Latin capital letter C with cedilla"
    "char: È entity: &Egrave;    text: Latin capital letter E with grave accent"
    "char: É entity: &Eacute;    text: Latin capital letter E with acute accent"
    "char: Ê entity: &Ecirc;     text: Latin capital letter E with circumflex"
    "char: Ë entity: &Euml;      text: Latin capital letter E with diaeresis"
    "char: Ì entity: &Igrave;    text: Latin capital letter I with grave accent"
    "char: Í entity: &Iacute;    text: Latin capital letter I with acute accent"
    "char: Î entity: &Icirc;     text: Latin capital letter I with circumflex"
    "char: Ï entity: &Iuml;      text: Latin capital letter I with diaeresis"
    "char: Ð entity: &ETH;       text: Latin capital letter Eth"
    "char: Ñ entity: &Ntilde;    text: Latin capital letter N with tilde"
    "char: Ò entity: &Ograve;    text: Latin capital letter O with grave accent"
    "char: Ó entity: &Oacute;    text: Latin capital letter O with acute accent"
    "char: Ô entity: &Ocirc;     text: Latin capital letter O with circumflex"
    "char: Õ entity: &Otilde;    text: Latin capital letter O with tilde"
    "char: Ö entity: &Ouml;      text: Latin capital letter O with diaeresis"
    "char: × entity: &times;     text: multiplication sign"
    "char: Ø entity: &Oslash;    text: Latin capital letter O with stroke (Latin capital letter O slash)"
    "char: Ù entity: &Ugrave;    text: Latin capital letter U with grave accent"
    "char: Ú entity: &Uacute;    text: Latin capital letter U with acute accent"
    "char: Û entity: &Ucirc;     text: Latin capital letter U with circumflex"
    "char: Ü entity: &Uuml;      text: Latin capital letter U with diaeresis"
    "char: Ý entity: &Yacute;    text: Latin capital letter Y with acute accent"
    "char: Þ entity: &THORN;     text: Latin capital letter THORN"
    "char: ß entity: &szlig;     text: Latin small letter sharp s (ess-zed); see German Eszett"
    "char: à entity: &agrave;    text: Latin small letter a with grave accent"
    "char: á entity: &aacute;    text: Latin small letter a with acute accent"
    "char: â entity: &acirc;     text: Latin small letter a with circumflex"
    "char: ã entity: &atilde;    text: Latin small letter a with tilde"
    "char: ä entity: &auml;      text: Latin small letter a with diaeresis"
    "char: å entity: &aring;     text: Latin small letter a with ring above"
    "char: æ entity: &aelig;     text: Latin small letter ae (Latin small ligature ae)"
    "char: ç entity: &ccedil;    text: Latin small letter c with cedilla"
    "char: è entity: &egrave;    text: Latin small letter e with grave accent"
    "char: é entity: &eacute;    text: Latin small letter e with acute accent"
    "char: ê entity: &ecirc;     text: Latin small letter e with circumflex"
    "char: ë entity: &euml;      text: Latin small letter e with diaeresis"
    "char: ì entity: &igrave;    text: Latin small letter i with grave accent"
    "char: í entity: &iacute;    text: Latin small letter i with acute accent"
    "char: î entity: &icirc;     text: Latin small letter i with circumflex"
    "char: ï entity: &iuml;      text: Latin small letter i with diaeresis"
    "char: ð entity: &eth;       text: Latin small letter eth"
    "char: ñ entity: &ntilde;    text: Latin small letter n with tilde"
    "char: ò entity: &ograve;    text: Latin small letter o with grave accent"
    "char: ó entity: &oacute;    text: Latin small letter o with acute accent"
    "char: ô entity: &ocirc;     text: Latin small letter o with circumflex"
    "char: õ entity: &otilde;    text: Latin small letter o with tilde"
    "char: ö entity: &ouml;      text: Latin small letter o with diaeresis"
    "char: ÷ entity: &divide;    text: division sign (obelus)"
    "char: ø entity: &oslash;    text: Latin small letter o with stroke (Latin small letter o slash)"
    "char: ù entity: &ugrave;    text: Latin small letter u with grave accent"
    "char: ú entity: &uacute;    text: Latin small letter u with acute accent"
    "char: û entity: &ucirc;     text: Latin small letter u with circumflex"
    "char: ü entity: &uuml;      text: Latin small letter u with diaeresis"
    "char: ý entity: &yacute;    text: Latin small letter y with acute accent"
    "char: þ entity: &thorn;     text: Latin small letter thorn"
    "char: ÿ entity: &yuml;      text: Latin small letter y with diaeresis"
    "char: Œ entity: &OElig;     text: Latin capital ligature oe"
    "char: œ entity: &oelig;     text: Latin small ligature oe"
    "char: Š entity: &Scaron;    text: Latin capital letter s with caron"
    "char: š entity: &scaron;    text: Latin small letter s with caron"
    "char: Ÿ entity: &Yuml;      text: Latin capital letter y with diaeresis"
    "char: ƒ entity: &fnof;      text: Latin small letter f with hook (function, florin)"
    "char: ˆ entity: &circ;      text: modifier letter circumflex accent"
    "char: ˜ entity: &tilde;     text: small tilde"
    "char: Α entity: &Alpha;     text: Greek capital letter Alpha"
    "char: Β entity: &Beta;      text: Greek capital letter Beta"
    "char: Γ entity: &Gamma;     text: Greek capital letter Gamma"
    "char: Δ entity: &Delta;     text: Greek capital letter Delta"
    "char: Ε entity: &Epsilon;   text: Greek capital letter Epsilon"
    "char: Ζ entity: &Zeta;      text: Greek capital letter Zeta"
    "char: Η entity: &Eta;       text: Greek capital letter Eta"
    "char: Θ entity: &Theta;     text: Greek capital letter Theta"
    "char: Ι entity: &Iota;      text: Greek capital letter Iota"
    "char: Κ entity: &Kappa;     text: Greek capital letter Kappa"
    "char: Λ entity: &Lambda;    text: Greek capital letter Lambda"
    "char: Μ entity: &Mu;        text: Greek capital letter Mu"
    "char: Ν entity: &Nu;        text: Greek capital letter Nu"
    "char: Ξ entity: &Xi;        text: Greek capital letter Xi"
    "char: Ο entity: &Omicron;   text: Greek capital letter Omicron"
    "char: Π entity: &Pi;        text: Greek capital letter Pi"
    "char: Ρ entity: &Rho;       text: Greek capital letter Rho"
    "char: Σ entity: &Sigma;     text: Greek capital letter Sigma"
    "char: Τ entity: &Tau;       text: Greek capital letter Tau"
    "char: Υ entity: &Upsilon;   text: Greek capital letter Upsilon"
    "char: Φ entity: &Phi;       text: Greek capital letter Phi"
    "char: Χ entity: &Chi;       text: Greek capital letter Chi"
    "char: Ψ entity: &Psi;       text: Greek capital letter Psi"
    "char: Ω entity: &Omega;     text: Greek capital letter Omega"
    "char: α entity: &alpha;     text: Greek small letter alpha"
    "char: β entity: &beta;      text: Greek small letter beta"
    "char: γ entity: &gamma;     text: Greek small letter gamma"
    "char: δ entity: &delta;     text: Greek small letter delta"
    "char: ε entity: &epsilon;   text: Greek small letter epsilon"
    "char: ζ entity: &zeta;      text: Greek small letter zeta"
    "char: η entity: &eta;       text: Greek small letter eta"
    "char: θ entity: &theta;     text: Greek small letter theta"
    "char: ι entity: &iota;      text: Greek small letter iota"
    "char: κ entity: &kappa;     text: Greek small letter kappa"
    "char: λ entity: &lambda;    text: Greek small letter lambda"
    "char: μ entity: &mu;        text: Greek small letter mu"
    "char: ν entity: &nu;        text: Greek small letter nu"
    "char: ξ entity: &xi;        text: Greek small letter xi"
    "char: ο entity: &omicron;   text: Greek small letter omicron"
    "char: π entity: &pi;        text: Greek small letter pi"
    "char: ρ entity: &rho;       text: Greek small letter rho"
    "char: ς entity: &sigmaf;    text: Greek small letter final sigma"
    "char: σ entity: &sigma;     text: Greek small letter sigma"
    "char: τ entity: &tau;       text: Greek small letter tau"
    "char: υ entity: &upsilon;   text: Greek small letter upsilon"
    "char: φ entity: &phi;       text: Greek small letter phi"
    "char: χ entity: &chi;       text: Greek small letter chi"
    "char: ψ entity: &psi;       text: Greek small letter psi"
    "char: ω entity: &omega;text: Greek small letter omega"
    "char: ϑ entity: &thetasym;  text: Greek theta symbol"
    "char: ϒ entity: &upsih;     text: Greek Upsilon with hook symbol"
    "char: ϖ entity: &piv;       text: Greek pi symbol"
    "char:   entity: &ensp;      text: en space"
    "char:   entity: &emsp;      text: em space"
    "char:   entity: &thinsp;    text: thin space"
    "char:   entity: &zwnj;      text: zero-width non-joiner"
    "char:   entity: &zwj;       text: zero-width joiner"
    "char:   entity: &lrm;       text: left-to-right mark"
    "char:   entity: &rlm;       text: right-to-left mark"
    "char: – entity: &ndash;     text: en dash"
    "char: — entity: &mdash;     text: em dash"
    "char: ‘ entity: &lsquo;     text: left single quotation mark"
    "char: ’ entity: &rsquo;     text: right single quotation mark"
    "char: ‚ entity: &sbquo;     text: single low-9 quotation mark"
    "char: “ entity: &ldquo;     text: left double quotation mark"
    "char: ” entity: &rdquo;     text: right double quotation mark"
    "char: „ entity: &bdquo;     text: double low-9 quotation mark"
    "char: † entity: &dagger;    text: dagger, obelisk"
    "char: ‡ entity: &Dagger;    text: double dagger, double obelisk"
    "char: • entity: &bull;      text: bullet (black small circle)"
    "char: … entity: &hellip;    text: horizontal ellipsis (three dot leader)"
    "char: ‰ entity: &permil;    text: per mille sign"
    "char: ′ entity: &prime;     text: prime (minutes, feet)"
    "char: ″ entity: &Prime;     text: double prime (seconds, inches)"
    "char: ‹ entity: &lsaquo;    text: single left-pointing angle quotation mark"
    "char: › entity: &rsaquo;    text: single right-pointing angle quotation mark"
    "char: ‾ entity: &oline;     text: overline (spacing overscore)"
    "char: ⁄ entity: &frasl;     text: fraction slash (solidus)"
    "char: € entity: &euro;      text: euro sign"
    "char: ℑ entity: &image;     text: black-letter capital I (imaginary part)"
    "char: ℘ entity: &weierp;    text: script capital P (power set, Weierstrass p)"
    "char: ℜ entity: &real;      text: black-letter capital R (real part symbol)"
    "char: ™ entity: &trade;     text: trademark symbol"
    "char: ℵ entity: &alefsym;   text: alef symbol (first transfinite cardinal)"
    "char: ← entity: &larr;      text: leftwards arrow"
    "char: ↑ entity: &uarr;      text: upwards arrow"
    "char: → entity: &rarr;      text: rightwards arrow"
    "char: ↓ entity: &darr;      text: downwards arrow"
    "char: ↔ entity: &harr;      text: left right arrow"
    "char: ↵ entity: &crarr;     text: downwards arrow with corner leftwards (carriage return)"
    "char: ⇐ entity: &lArr;      text: leftwards double arrow"
    "char: ⇑ entity: &uArr;      text: upwards double arrow"
    "char: ⇒ entity: &rArr;      text: rightwards double arrow"
    "char: ⇓ entity: &dArr;      text: downwards double arrow"
    "char: ⇔ entity: &hArr;      text: left right double arrow"
    "char: ∀ entity: &forall;    text: for all"
    "char: ∂ entity: &part;      text: partial differential"
    "char: ∃ entity: &exist;     text: there exists"
    "char: ∅ entity: &empty;     text: empty set (null set); see also ⌀"
    "char: ∇ entity: &nabla;     text: del or nabla (vector differential operator)"
    "char: ∈ entity: &isin;      text: element of"
    "char: ∉ entity: &notin;     text: not an element of"
    "char: ∋ entity: &ni;        text: contains as member"
    "char: ∏ entity: &prod;      text: n-ary product (product sign)"
    "char: ∑ entity: &sum;       text: n-ary summation"
    "char: − entity: &minus;     text: minus sign"
    "char: ∗ entity: &lowast;    text: asterisk operator"
    "char: √ entity: &radic;     text: square root (radical sign)"
    "char: ∝ entity: &prop;      text: proportional to"
    "char: ∞ entity: &infin;     text: infinity"
    "char: ∠ entity: &ang;       text: angle"
    "char: ∧ entity: &and;       text: logical and (wedge)"
    "char: ∨ entity: &or;        text: logical or (vee)"
    "char: ∩ entity: &cap;       text: intersection (cap)"
    "char: ∪ entity: &cup;       text: union (cup)"
    "char: ∫ entity: &int;       text: integral"
    "char: ∴ entity: &there4;    text: therefore sign"
    "char: ∼ entity: &sim;       text: tilde operator (varies with, similar to)"
    "char: ≅ entity: &cong;      text: congruent to"
    "char: ≈ entity: &asymp;     text: almost equal to (asymptotic to)"
    "char: ≠ entity: &ne;        text: not equal to"
    "char: ≡ entity: &equiv;     text: identical to; sometimes used for 'equivalent to'"
    "char: ≤ entity: &le;        text: less-than or equal to"
    "char: ≥ entity: &ge;        text: greater-than or equal to"
    "char: ⊂ entity: &sub;       text: subset of (as in Vim ⊂ Emacs)"
    "char: ⊃ entity: &sup;       text: superset of (as in Emacs ⊃ Vim)"
    "char: ⊄ entity: &nsub;      text: not a subset of (as in Emacs ⊄ Vim)"
    "char: ⊆ entity: &sube;      text: subset of or equal to"
    "char: ⊇ entity: &supe;      text: superset of or equal to (as in Emacs ⊇ Vim)"
    "char: ⊕ entity: &oplus;     text: circled plus (direct sum)"
    "char: ⊗ entity: &otimes;    text: circled times (vector product)"
    "char: ⊥ entity: &perp;      text: up tack (orthogonal to, perpendicular)"
    "char: ⋅ entity: &sdot;      text: dot operator"
    "char: ⌈ entity: &lceil;     text: left ceiling (APL upstile)"
    "char: ⌉ entity: &rceil;     text: right ceiling"
    "char: ⌊ entity: &lfloor;    text: left floor (APL downstile)"
    "char: ⌋ entity: &rfloor;    text: right floor"
    "char: 〈 entity: &lang;     text: left-pointing angle bracket (bra)"
    "char: 〉 entity: &rang;     text: right-pointing angle bracket (ket)"
    "char: ◊ entity: &loz;       text: lozenge"
    "char: ♠ entity: &spades;    text: black spade suit"
    "char: ♣ entity: &clubs;     text: black club suit (shamrock)"
    "char: ♥ entity: &hearts;    text: black heart suit (valentine)"
    "char: ♦ entity: &diams;     text: black diamond suit")
  "List of HTML entities with helpful text notes.")

(defun aero/html-entity-select ()
  "Select an html entry from the list."
  (interactive)
  (cond
   ((and (fboundp #'ivy-completing-read)
         (bound-and-true-p ivy-mode))
    (ivy-completing-read "HTML Entity: "
                         aero/html-entity-list
                         nil nil
                         (char-to-string (char-after (point)))))
   (t (completing-read "HTML Entity: "
                       aero/html-entity-list
                       nil nil
                       (char-to-string (char-after (point)))))))

(defun aero/html-entity-insert ()
  "Select and insert an html entity."
  (interactive)
  (let ((entry (aero/html-entity-select)))
    (string-match "entity: \\(&.*?;\\)" entry)
    (insert (match-string 1 entry))))

(eval-after-load 'general
  (aero-leader-def
    "th" 'aero/html-entity-insert
    "tH" 'aero/html-entity-select))


(provide 'aero-web)
