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
 ;; Editing
 '(indent-tabs-mode nil)
 '(mark-ring-max 32)
 '(global-mark-ring-max 32)
 '(set-mark-command-repeat-pop t)
 '(delete-selection-mode t)
 '(electric-pair-mode t)
 ;; Frames
 '(initial-frame-alist
   '((height . 43) (width . 100) (font . "Consolas-11")))
 '(default-frame-alist initial-frame-alist)
 '(frame-title-format
   '("Emacs@" (:eval (system-name)) " - " (buffer-file-name "%f" "%b")))
 '(cursor-in-non-selected-windows nil)
 '(indicate-buffer-boundaries '((top . left) (bottom . left)))
 '(menu-bar-mode (display-graphic-p))
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 ;; Help
 '(text-quoting-style 'straight)
 ;; I18n
 '(eol-mnemonic-dos "[dos]")
 '(eol-mnemonic-mac "[mac]")
 '(eol-mnemonic-unix "[unix]")
 ;; Initialization
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 ;; Killing
 '(backward-delete-char-untabify-method nil)
 '(kill-do-not-save-duplicates t)
 '(yank-excluded-properties t)
 '(kill-ring-max 100)
 ;; Lisp
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 ;; Matching
 '(query-replace-skip-read-only t)
 ;; Minibuffer
 '(echo-keystrokes 0.5)
 '(history-delete-duplicates t)
 '(completion-ignore-case t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(minibuffer-electric-default-mode t)
 ;; Mode Line
 '(column-number-mode t)
 '(size-indication-mode t)
 ;; Mouse
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control))))
 ;; Paragraphs
 '(bidi-display-reordering nil)
 '(bidi-paragraph-direction 'left-to-right)
 ;; Windows
 '(scroll-conservatively 24)
 '(scroll-step 1)
 '(scroll-preserve-screen-position t)
 '(next-screen-context-lines 1)
 '(hscroll-margin 0)
 '(hscroll-step 1))

(dir-locals-set-class-variables
 'shared-library
 '((nil . ((buffer-read-only . t)))))

(dir-locals-set-directory-class
 (expand-file-name "../lisp" data-directory) 'shared-library)

;; Packages
;; --------

(require 'package)

(dolist (archive '(("melpa-stable" . "https://stable.melpa.org/packages/")
                   ("melpa"        . "https://melpa.org/packages/")))
  (add-to-list 'package-archives archive))

(custom-set-variables
 '(package-user-dir
   (expand-file-name (format "~/.local/share/emacs/%s.%s/lisp/elpa"
                             emacs-major-version emacs-minor-version))))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Keymaps
;; -------

(keyboard-translate ?\b ?\d)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
