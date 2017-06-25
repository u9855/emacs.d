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

(coding-system-put 'cp932 :mnemonic ?P)
(dolist (coding-system (coding-system-list))
  ;; Unicode with BOM
  (if (string-suffix-p "with-signature" (symbol-name coding-system))
      (coding-system-put coding-system :mnemonic ?u)))

(set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
                      'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)

(let ((table (assq 'ja_JP cjk-char-width-table-list)))
  (when table
    (setcdr table
            '(nil
              ;; JIS X 0208 の文字集合は JIS X 2013 で規定されている。
              (japanese-jisx0213.2004-1 (#x2121 . #x2d7e))
              (japanese-jisx0212        (#x2121 . #x2b7e))
              (cp932-2-byte             (#x8140 . #x879f))
              (unicode                  (#x02c4 . #x02c4) (#x02c9 . #x02cb)
                                        (#x02cd . #x02cd) (#x02df . #x02df)
                                        (#x2024 . #x2027) (#x2035 . #x2035)
                                        (#x203e . #x203e) (#x2074 . #x2074)
                                        (#x207f . #x207f) (#x2081 . #x2084)
                                        (#x2105 . #x2105) (#x2109 . #x2109)
                                        (#x2126 . #x2126) (#x215b . #x215e)
                                        (#x21b8 . #x21b9) (#x220f . #x220f)
                                        (#x2215 . #x2215) (#x2223 . #x2223)
                                        (#x224c . #x224c) (#x226e . #x226f)
                                        (#x2299 . #x2299) (#x2550 . #x2573)
                                        (#x2580 . #x258f) (#x2592 . #x2595)
                                        (#x25a3 . #x25a9) (#x25e2 . #x25e5)
                                        (#x2609 . #x2609) (#x2614 . #x2615)
                                        (#x261c . #x261c) (#x273d . #x273d)
                                        (#xe000 . #xf8ff) (#xfe00 . #xfe0f)
                                        (#xfffd . #xfffd))))
    (use-cjk-char-width-table 'ja_JP)))

;; Customizations
;; --------------

(custom-set-variables
 ;; 設定を一時ファイルに書き出して init.el が書き換えられるのを防ぐ。
 '(custom-file (or custom-file (make-temp-file "custom" nil ".el")))
 ;; Display
 '(truncate-partial-width-windows t)
 ;; Initialization
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 ;; Minibuffer
 '(echo-keystrokes 0.5)
 ;; Paragraphs
 '(bidi-display-reordering nil)
 '(bidi-paragraph-direction 'left-to-right))

;; Keymaps
;; -------

(keyboard-translate ?\b ?\d)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
