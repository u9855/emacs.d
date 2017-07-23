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
 ;; Files
 '(make-backup-files nil)
 '(auto-save-default nil)
 '(auto-save-list-file-prefix nil)
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
 '(help-window-select t)
 '(text-quoting-style 'straight)
 ;; I18n
 '(eol-mnemonic-dos "[dos]")
 '(eol-mnemonic-mac "[mac]")
 '(eol-mnemonic-unix "[unix]")
 ;; Initialization
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 ;; Isearch
 '(search-ring-max 32)
 '(regexp-search-ring-max 32)
 '(isearch-allow-scroll t)
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
 '(blink-matching-paren nil)
 '(show-paren-mode t)
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

(define-advice package-install-from-archive
    (:around (fn pkg-desc) disable-dir-local-variables)
  "Bind `enable-dir-local-variables' to nil then install a package.

`package-user-dir' に対してローカルディレクトリ変数 `buffer-read-only' の値が
non-nil に設定されているとインストールに失敗するので一時的に無効にする。"
  (let ((enable-dir-local-variables nil))
    (funcall fn pkg-desc)))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(custom-set-variables '(use-package-always-ensure t))

(use-package dash
  :config
  (use-package dash-functional)

  (dash-enable-font-lock))

(use-package region-bindings-mode
  :diminish region-bindings-mode
  :config (region-bindings-mode-enable))

(use-package anzu)

(use-package auto-save-buffers-enhanced
  :config
  (custom-set-variables
   '(auto-save-buffers-enhanced-interval 2)
   '(auto-save-buffers-enhanced-include-regexps nil))

  (define-advice auto-save-buffers-enhanced-save-buffers
      (:around (fn) save-silently)
    "Avoid messages when saving files."
    (let ((save-silently t))
      (funcall fn)))

  (defun auto-save-buffers-enhanced-git-repository-p ()
    "Return non-nil if `buffer-file-name' is under git control."
    (and (executable-find "git")
         (with-temp-buffer
           (zerop (call-process "git" nil '(t nil) nil
                                "rev-parse" "--is-inside-work-tree")))))

  (defun auto-save-buffers-enhanced-add-git-repository ()
    "Add a regexp that matches a file under git control to
`auto-save-buffers-enhanced-include-regexps'."
    (if (auto-save-buffers-enhanced-git-repository-p)
        (add-to-list 'auto-save-buffers-enhanced-include-regexps
                     (concat "^" (regexp-quote (file-name-directory
                                                (buffer-file-name)))))))

  (add-hook 'find-file-hook 'auto-save-buffers-enhanced-add-git-repository)

  (auto-save-buffers-enhanced t))

(use-package avy
  :bind (:map goto-map
         (":" . avy-goto-char)
         ("w" . avy-goto-word-1))
  :config
  (custom-set-variables
   '(avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p
                ?z ?x ?c ?v ?b ?n ?m)))

  (use-package avy-migemo
    :if (executable-find "cmigemo")
    :config (custom-set-variables '(avy-migemo-mode t))))

(use-package avy-zap
  :bind ([remap zap-to-char] . avy-zap-to-char))

(use-package cp5022x
  :config (define-coding-system-alias 'euc-jp 'cp51932))

(use-package dabbrev
  :defer t
  :config
  (defvar dabbrev-additional-ignored-buffer-names
    '("*Backtrace*" "*Completions*" "*Compile-Log*" "*Occur*" "*trace-output*"
      "*vc*" "*vc-diff*"))

  (custom-set-variables
   '(dabbrev-abbrev-skip-leading-regexp "[!$%&'*<>`]")
   '(dabbrev-upcase-means-case-search t)
   '(dabbrev-ignored-buffer-names
     (-union dabbrev-ignored-buffer-names
             dabbrev-additional-ignored-buffer-names))
   '(dabbrev-ignored-buffer-regexps
     (-union dabbrev-ignored-buffer-regexps '("^ "))))

  (define-advice dabbrev-expand (:around (fn arg) support-japanese)
    "Support expanding Japanese abbreviation."
    (let ((dabbrev-abbrev-char-regexp
           (unless (bobp)
             (let ((category (char-category-set (char-before))))
               (cond ((aref category ?a) "\\sw\\|\\s_") ; ASCII
                     ((aref category ?A) "\\cA")        ; Multibyte Alnum
                     ((aref category ?C) "\\cC")        ; Kanji
                     ((aref category ?K) "\\cK")        ; Katakana
                     ((aref category ?H) "\\cH")        ; Hiragana
                     ((aref category ?k) "\\ck")        ; Hankaku Katakana
                     ((aref category ?r) "\\cr")        ; Japanese Roman
                     ((aref category ?j) "\\cj")        ; Japanese
                     (t dabbrev-abbrev-char-regexp))))))
      (funcall fn arg))))

(use-package easy-kill
  :bind ([remap kill-ring-save] . easy-kill)
  :config
  (custom-set-variables
   '(easy-kill-alist (-union '((?. symbol " ")) easy-kill-alist))
   '(easy-kill-unhighlight-key (kbd "RET"))
   '(easy-kill-try-things '(url email symbol sexp line))
   '(easy-mark-try-things easy-kill-try-things))

  (bind-keys :map easy-kill-base-map
             ("C-d" . easy-kill-delete-region)
             ("C-w" . easy-kill-region)))

(use-package easy-repeat
  :config
  (defvar easy-repeat-additional-commands
    '(previous-buffer scroll-left scroll-right scroll-other-window-down
      backward-up-list down-list backward-kill-sentence backward-kill-sexp
      enlarge-window shrink-window enlarge-window-horizontally
      shrink-window-horizontally winner-undo goto-last-change
      goto-last-change-reverse er/expand-region er/contract-region))

  (custom-set-variables
   '(easy-repeat-command-list
     (-union (-difference
              easy-repeat-command-list
              '(recenter-top-bottom kill-buffer scroll-up-command
                scroll-down-command beginning-of-defun end-of-defun))
             easy-repeat-additional-commands))
   '(easy-repeat-mode t)))

(use-package expand-region
  :bind (:map mode-specific-map
         ("C-@" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package ffap
  :config
  (custom-set-variables
   '(ffap-ftp-regexp nil)
   '(ffap-url-unwrap-remote nil)
   '(ffap-rfs-regexp nil)
   '(ffap-machine-p-known 'accept))

  (add-to-list 'ffap-alist '(lisp-interaction-mode . ffap-el-mode))

  (ffap-bindings))

(use-package goto-chg
  :bind (:map goto-map
         (";" . goto-last-change)
         ("," . goto-last-change-reverse)))

(use-package imenu
  :bind (:map search-map ("i" . imenu))
  :config (add-hook 'imenu-after-jump-hook 'recenter))

(use-package isearch-dabbrev
  :bind (:map isearch-mode-map ("M-/" . isearch-dabbrev-expand)))

(use-package log-edit
  :defer t
  :config (custom-set-variables
           '(log-edit-hook '(log-edit-insert-filenames-without-changelog))))

(use-package migemo
  :if (executable-find "cmigemo")
  :config
  (custom-set-variables
   '(migemo-command "cmigemo")
   '(migemo-options '("-e" "-q"))
   '(migemo-isearch-enable-p nil)
   '(migemo-dictionary
     (expand-file-name "~/.local/share/migemo/utf-8/migemo-dict"))
   '(migemo-user-dictionary nil)
   '(migemo-regex-dictionary nil)
   '(migemo-coding-system 'utf-8-unix)
   '(migemo-use-frequent-pattern-alist t)
   '(migemo-frequent-pattern-alist-file
     (expand-file-name "~/.cache/emacs/migemo-frequent")))

  (bind-key "C-k" 'migemo-isearch-toggle-migemo isearch-mode-map)

  (migemo-init))

(use-package multiple-cursors-core
  :ensure multiple-cursors
  :bind (("M-*" . mc/mark-all-dwim)
         :map region-bindings-mode-map
         ("C-;" . mc/mark-next-like-this)
         ("C-," . mc/mark-previous-like-this))
  :config
  (custom-set-variables '(mc/insert-numbers-default 1))

  (bind-keys :map mc/keymap
             ("C-M-;"   . mc/skip-to-next-like-this)
             ("C-M-,"   . mc/skip-to-previous-like-this)
             ("C-c C-;" . mc/unmark-next-like-this)
             ("C-c C-," . mc/unmark-previous-like-this)))

(use-package recentf
  :config
  (custom-set-variables
   '(recentf-max-saved-items 100)
   '(recentf-save-file (expand-file-name "~/.cache/emacs/recentf"))
   '(recentf-exclude
     (-union (list (regexp-quote (expand-file-name "~/.cache")))
             recentf-exclude))
   '(recentf-menu-filter 'recentf-arrange-by-dir)
   '(recentf-mode t))

  (bind-key "f" 'recentf-open-files mode-specific-map))

(use-package shackle
  :config
  (custom-set-variables
   '(shackle-rules (-union '((occur-mode :align t)
                             ("*vc-log*" :align t))
                           shackle-rules))
   '(shackle-mode t)))

(use-package solarized-theme
  :config
  (custom-set-variables
   '(solarized-distinct-fringe-background t)
   '(solarized-use-less-bold t)
   '(solarized-use-more-italic t)
   '(solarized-height-minus-1 1.0)
   '(solarized-height-plus-1 1.0)
   '(solarized-height-plus-2 1.0)
   '(solarized-height-plus-3 1.0)
   '(solarized-height-plus-4 1.0))

  (load-theme 'solarized-light t)

  (custom-theme-set-faces
   'solarized-light
   '(font-lock-comment-face ((t (:inherit font-lock-comment-delimiter-face))))
   '(whitespace-tab ((t (:background "#eee8d5"))))
   '(whitespace-trailing ((t (:background "#eee8d5"))))))

(use-package switch-window
  :bind (:map ctl-x-4-map
         ("0" . switch-window-then-delete)
         ("1" . switch-window-then-maximize)
         ("t" . switch-window-then-swap-buffer)
         :map goto-map
         ("o" . switch-window))
  :config
  (custom-set-variables
   '(switch-window-timeout 2)
   '(switch-window-shortcut-style 'qwerty)
   '(switch-window-qwerty-shortcuts
     '("a" "s" "d" "f" "j" "k" "l" ";" "q" "w" "e" "r" "u" "i" "o" "p"))))

(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (custom-set-variables
   '(whitespace-style '(face tabs trailing))
   '(whitespace-trailing-regexp "\\([ \t\u00A0\u3000]+\\)$")
   '(global-whitespace-mode t)))

(use-package winner
  :init (custom-set-variables '(winner-dont-bind-my-keys t))
  :config
  (custom-set-variables '(winner-mode t))

  (with-eval-after-load 'xref
    (add-to-list 'winner-boring-buffers xref-buffer-name))

  (bind-keys :map ctl-x-map
             ("w"   . winner-undo)
             ("M-w" . winner-redo)))

;; Commands
;; --------

(advice-add 'yes-or-no-p :override 'y-or-n-p)

(--each '(scroll-left downcase-region upcase-region)
  (put it 'disabled nil))

(defun beginning-of-string (&optional arg)
  "Move point to the beginning of string object at point."
  (interactive "^p")
  (let ((syntax (syntax-ppss)))
    (if (nth 3 syntax)
        (goto-char (nth 8 syntax)))))

(--each '(down-list forward-list)
  (advice-add it :before 'beginning-of-string))

(defun x-select-inhibit-accessing-clipboard (fn arg)
  "Inhibit kill commands from accessing the clipboard."
  (let ((x-select-enable-clipboard nil))
    (funcall fn arg)))

(--each '(kill-word backward-kill-word)
  (advice-add it :around 'x-select-inhibit-accessing-clipboard))

(defun duplicate-line (arg)
  "Duplicate the current line ARG times."
  (interactive "p")
  (save-excursion
    (goto-char (line-end-position))
    (let ((line (buffer-substring (line-beginning-position) (point))))
      (--dotimes arg (insert "\n" line)))))

(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-mark-and-excursion
   (mark-defun)
   (indent-region (region-beginning) (region-end))))

;; Keymaps
;; -------

(keyboard-translate ?\b ?\d)

(bind-keys :map global-map
           ("C-w"   . backward-kill-word)
           ("M-n"   . scroll-up-line)
           ("M-p"   . scroll-down-line)
           ("M-SPC" . cycle-spacing)
           ("C-M-y" . duplicate-line)
           :map ctl-x-map
           ("-" . shrink-window)
           :map esc-map
           ("M-v" . scroll-other-window-down)
           :map region-bindings-mode-map
           ("C-w" . kill-region)
           ("M-c" . capitalize-region)
           ("M-l" . downcase-region)
           ("M-u" . upcase-region))

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
