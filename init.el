;;; init.el --- The initialization file for GNU Emacs

;;; Commentary:

;; The file requires GNU Emacs version 25.1 or later.

;;; Code:

;; I18n
;; ----

(set-language-environment "Japanese")

(prefer-coding-system
 (if (eq system-type 'windows-nt) 'cp932-dos 'utf-8-unix))
(set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)

;; Keymaps
;; -------

(keyboard-translate ?\b ?\d)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
