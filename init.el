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
 '(word-combining-categories
   (let (categories)
     (dolist (category word-combining-categories categories)
       (unless (member category '((?C . ?H) (?C . ?K)))
         (push category categories)))))
 '(word-separating-categories
     (dolist (category
              '((?H . ?k) (?K . ?H) (?K . ?k) (?k . ?H) (?k . ?K))
              word-separating-categories)
       (add-to-list 'word-separating-categories category)))
 ;; Initialization
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 ;; Isearch
 '(search-ring-max 32)
 '(regexp-search-ring-max 32)
 '(search-whitespace-regexp nil)
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

(dolist
    (target '(japanese-jisx0212 japanese-jisx0213.2004-1 katakana-jisx0201))
  (set-fontset-font t target (font-spec :family "MeiryoKe_Console")))
(set-fontset-font t 'unicode (font-spec :family "MeiryoKe_Console") nil 'append)

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

(custom-set-variables
 '(use-package-enable-imenu-support t)
 '(use-package-always-ensure t))

(use-package dash
  :config
  (use-package dash-functional)

  (dash-enable-font-lock))

(use-package region-bindings-mode
  :diminish region-bindings-mode
  :config (region-bindings-mode-enable))

(use-package add-node-modules-path
  :if (executable-find "node")
  :after js
  :config (add-hook 'js-mode-hook 'add-node-modules-path))

(use-package ag
  :if (executable-find "ag")
  :defer t
  :config
  (custom-set-variables
   '(ag-arguments (remove "--stats" ag-arguments))
   '(ag-group-matches nil)
   '(ag-highlight-search t)
   '(ag-reuse-buffers t)))

(use-package anzu
  :diminish anzu-mode
  :config
  (custom-set-variables
   '(anzu-minimum-input-length 3)
   '(global-anzu-mode t))

  (custom-set-faces
   '(anzu-match-1 ((t (:foreground "#fdf6e3" :background "#2aa198"))))
   '(anzu-match-2 ((t (:foreground "#fdf6e3" :background "#859900"))))
   '(anzu-match-3 ((t (:foreground "#fdf6e3" :background "#b58900"))))
   '(anzu-replace-to ((t (:foreground "#d33682")))))

  (bind-keys :map global-map
             ([remap query-replace]        . anzu-query-replace)
             ([remap query-replace-regexp] . anzu-query-replace-regexp)
             :map isearch-mode-map
             ([remap isearch-query-replace] . anzu-isearch-query-replace)
             ([remap isearch-query-replace-regexp] .
              anzu-isearch-query-replace-regexp)))

(use-package auto-save-buffers-enhanced
  :config
  (custom-set-variables
   '(auto-save-buffers-enhanced-interval 1)
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

(use-package auto-virtualenvwrapper
  :after python
  :config
  (custom-set-variables '(auto-virtualenvwrapper-dir venv-location))

  (add-hook 'python-mode-hook 'auto-virtualenvwrapper-activate))

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

(use-package bm
  :bind (:map ctl-x-r-map ("@" . bm-toggle))
  :config
  (custom-set-variables
   '(bm-highlight-style (if window-system
                            'bm-highlight-only-fringe
                          'bm-highlight-only-line))
   '(bm-recenter t))

  (bind-keys :map goto-map
             ("[" . bm-next)
             ("]" . bm-previous)))

(use-package bookmark
  :defer t
  :config
  (custom-set-variables
   '(bookmark-save-flag 1)
   '(bookmark-default-file (expand-file-name "~/.cache/emacs/bookmarks"))))

(use-package browse-at-remote
  :bind (:map vc-prefix-map ("C-v" . browse-at-remote)))

(use-package company
  :diminish company-mode
  :config
  (custom-set-variables
   '(company-frontends
     '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
       company-preview-if-just-one-frontend
       company-echo-metadata-frontend))
   '(company-transformers
     (-union '(company-sort-by-occurrence) company-transformers))
   '(company-idle-delay 0.2)
   '(global-company-mode t))

  (defun company-add-buffer-local-backend (backend)
    (add-to-list (make-local-variable 'company-backends) backend))

  (use-package company-jedi
    :after python
    :config
    (custom-set-variables
     '(jedi:environment-root
       (expand-file-name "~/.local/share/emacs/virtualenvs/jedi"))
     '(jedi:server-command (jedi:-env-server-command))
     '(jedi:complete-on-dot t)
     '(jedi:use-shortcuts t))

    ;; インストール時にエラーが出るけど気にしない！
    (unless jedi:server-command (jedi:install-server))

    (add-hook 'python-mode-hook 'jedi:setup)

    (add-hook 'python-mode-hook
              (-partial 'company-add-buffer-local-backend 'company-jedi)))

  (use-package company-statistics
    :config
    (custom-set-variables
     '(company-statistics-auto-save nil)
     '(company-statistics-auto-restore nil)
     '(company-statistics-mode t)))

  (use-package company-tern
    :after tern
    :config
    (custom-set-variables '(company-tern-property-marker nil))

    (add-hook 'tern-mode-hook
              (-partial 'company-add-buffer-local-backend 'company-tern)))

  (use-package company-web-html
    :ensure company-web
    :after web-mode
    :config
    (add-hook 'web-mode-hook
              (-partial 'company-add-buffer-local-backend 'company-web-html)))

  (bind-keys :map company-mode-map
             ("C-M-i" . company-complete)
             :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-s" . company-filter-candidates)
             :map company-search-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous))

  (unbind-key "C-w" company-active-map))

(use-package cp5022x
  :config (define-coding-system-alias 'euc-jp 'cp51932))

(use-package css-mode
  :defer t
  :config (custom-set-variables '(css-indent-offset 2)))

(use-package dabbrev
  :defer t
  :config
  (defvar dabbrev-additional-ignored-buffer-names
    '("*Backtrace*" "*Completions*" "*Compile-Log*" "*Occur*" "*WoMan-Log*"
      "*trace-output*" "*vc*" "*vc-diff*"))

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

(use-package dired
  :defer t
  :ensure nil
  :config
  (custom-set-variables
   '(dired-listing-switches "-AFhlv")
   '(dired-keep-marker-rename nil)
   '(dired-keep-marker-copy nil)
   '(dired-keep-marker-hardlink nil)
   '(dired-keep-marker-symlink nil)
   '(dired-dwim-target t)
   '(dired-hide-details-hide-information-lines nil)
   '(dired-auto-revert-buffer 'dired-directory-changed-p))

  (use-package dired-x
    :ensure nil
    :init
    (custom-set-variables
     '(dired-bind-jump nil)
     '(dired-bind-man nil)
     '(dired-bind-info nil)))

  (use-package dired-hide-dotfiles
    :hook (dired-mode . dired-hide-dotfiles-mode)
    :bind (:map dired-mode-map ("C-." . dired-hide-dotfiles-mode))))

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
      goto-last-change-reverse er/expand-region er/contract-region
      bm-next bm-previous perspeen-next-ws perspeen-previous-ws))

  (custom-set-variables
   '(easy-repeat-command-list
     (-union (-difference
              easy-repeat-command-list
              '(recenter-top-bottom kill-buffer scroll-up-command
                scroll-down-command beginning-of-defun end-of-defun))
             easy-repeat-additional-commands))
   '(easy-repeat-mode t)))

(use-package ediff
  :defer t
  :config
  (custom-set-variables
   '(ediff-make-buffers-readonly-at-startup t)
   '(ediff-custom-diff-options "-u")
   '(ediff-window-setup-function 'ediff-setup-windows-plain)))

(use-package edit-list)

;; Cheat Sheet
;; https://docs.emmet.io/cheat-sheet/
(use-package emmet-mode
  :hook (css-mode html-mode web-mode)
  :config (custom-set-variables
           '(emmet-indentation 2)
           '(emmet-self-closing-tag-style "")
           '(emmet-insert-flash-time 0.01)
           '(emmet-move-cursor-between-quotes t)))

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

(use-package filecache
  :if (package-installed-p 'helm)
  :config (add-hook 'after-init-hook
                    (lambda () (file-cache-add-directory-list load-path))))

(use-package find-dired
  :defer t
  :config
  (custom-set-variables
   '(find-ls-option '("-exec ls -Fdhlv {} +" . "-Fdhlv"))
   '(find-ls-subdir-switches dired-listing-switches)))

(use-package flycheck
  :init (custom-set-variables '(flycheck-keymap-prefix (kbd "C-c `")))
  :config
  (custom-set-variables
   '(flycheck-check-syntax-automatically '(save mode-enabled))
   '(flycheck-global-modes '(not emacs-lisp-mode lisp-interaction-mode))
   '(flycheck-flake8-maximum-complexity 10)
   '(js2-mode-show-parse-errors nil)
   '(js2-mode-show-strict-warnings nil)
   '(js2-include-browser-externs nil))

  ;; UnicodeDecodeError の対処。
  (add-to-list 'process-coding-system-alist
              '("flake8" undecided-dos . utf-8-dos))

  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package goto-chg
  :bind (:map goto-map
         (";" . goto-last-change)
         ("," . goto-last-change-reverse)))

(use-package helm
  :config
  (custom-set-variables
   '(helm-input-idle-delay 0.1)
   '(helm-candidate-separator (make-string 72 ?-))
   '(helm-display-header-line nil)
   '(helm-inherit-input-method nil)
   '(helm-bookmark-show-location t)
   '(helm-buffer-max-length 24)
   '(helm-M-x-requires-pattern 2)
   '(helm-command-prefix-key "C-c c")
   '(helm-turn-on-show-completion nil)
   '(helm-show-completion-use-special-display nil)
   '(helm-locate-library-fuzzy-match nil)
   '(helm-ff-fuzzy-matching nil)
   '(helm-ff-skip-boring-files t)
   '(helm-ff-guess-ffap-filenames t)
   '(helm-imenu-execute-action-at-once-if-one nil)
   '(helm-man-or-woman-function 'woman))

  (advice-add 'helm-default-display-buffer :override 'display-buffer)

   (with-eval-after-load 'helm-buffers
     (custom-set-variables
      '(helm-boring-buffer-regexp-list
        (-union '("^\\*Completions\\*$" "^\\*Ibuffer\\*$" "^\\*Messages\\*$"
                  "^\\*Woman-Log\\*$" "^\\*magit[^:]+:" "^\\*sdic\\*$"
                  "^\\*vc\\(-.+\\)*\\*$" "^\\*xref\\*$")
                helm-boring-buffer-regexp-list))))

   (with-eval-after-load 'helm-for-files
     (custom-set-variables
      '(helm-for-files-preferred-list
        (-difference helm-for-files-preferred-list
                     '(helm-source-buffers-list helm-source-locate)))))

   (define-advice helm-buffers-sort-transformer
       (:override (candidates source) disable-sort)
     candidates)

   (require 'helm-config)

   (add-hook 'after-init-hook 'helm-mode)

   (with-eval-after-load 'helm-mode
     (custom-set-variables
      '(helm-completing-read-handlers-alist
        (-union '((dired-at-point     . nil)
                  (find-file-at-point . nil)
                  (ffap               . nil))
                helm-completing-read-handlers-alist)))

     (diminish 'helm-mode))

   (define-advice read-file-name
       (:around (fn &rest args) disable-helm-completion)
     (let ((helm-completing-read-handlers-alist
            (list (cons (or (helm-this-command) this-command) nil))))
       (apply fn args)))

   (use-package helm-ag
     :if (-any? 'executable-find '("ag" "rg"))
     :bind ([remap helm-do-grep-ag] . helm-do-ag)
     :config
     (custom-set-variables
      '(helm-ag-base-command (if (executable-find "rg")
                                 "rg --line-number --smart-case --no-heading"
                               "ag --numbers --smart-case --nogroup")))

     (defun helm-ag--set-process-coding-system (fn)
       "Prevent garbled characters at input and output of FN."
       (let ((default-process-coding-system
               (cons 'utf-8-unix (cdr default-process-coding-system)))
             (buffer-file-coding-system nil))
         (funcall fn)))

     (--each '(helm-ag--init helm-ag--do-ag-candidate-process)
       (advice-add it :around 'helm-ag--set-process-coding-system)))

   (use-package helm-bm
     :after bm
     :config
     (custom-set-variables
      '(helm-source-bm (remove '(multiline) helm-source-bm)))

     (with-eval-after-load 'helm-bookmark
       (add-to-list 'helm-bookmark-default-filtered-sources 'helm-source-bm)))

   (use-package helm-css-scss
     :after css-mode
     :config
     (custom-set-variables
      '(helm-css-scss-split-window-function 'helm-default-display-buffer))

     (bind-keys :map css-mode-map
                ([remap imenu]                     . helm-css-scss)
                ([remap helm-imenu-in-all-buffers] . helm-css-scss-multi)
                :map helm-css-scss-map
                ("M-i" . helm-css-scss-multi-from-helm-css-scss)))

   (use-package helm-flycheck
     :after flycheck
     :bind (:map flycheck-mode-map ("M-s `" . helm-flycheck)))

   (bind-keys :map global-map
              ([remap bookmark-jump]            . helm-filtered-bookmarks)
              ([remap switch-to-buffer]         . helm-buffers-list)
              ([remap execute-extended-command] . helm-M-x)
              ([remap imenu]                    . helm-imenu)
              ([remap occur]                    . helm-occur)
              ([remap isearch-occur]            . helm-occur-from-isearch)
              ([remap yank-pop]                 . helm-show-kill-ring)
              :map mode-specific-map
              ("f" . helm-multi-files)
              :map search-map
              ("M-i" . helm-imenu-in-all-buffers)
              :map isearch-mode-map
              ("M-s M-o" . helm-multi-occur-from-isearch)
              :map helm-command-map
              ("TAB" . helm-lisp-completion-at-point)))

(use-package ibuffer
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :bind ([remap list-buffers] . ibuffer)
  :config
  (custom-set-variables
   '(ibuffer-formats
     '((mark modified read-only
        " " (name 20 20 :left :elide) " " (size 9 -1 :right)
        " " (mode 16 16 :left :elide) " " filename-and-process)
       (mark modified read-only
             " " (name 30 -1 :left :elide) " " filename-and-process)))
   '(ibuffer-default-sorting-mode 'filename/process)
   '(ibuffer-maybe-show-predicates
     (-union '("^\\*Completions\\*$" "^\\*Messages\\*$" "^\\*WoMan-Log\\*$"
               "^\\*epc" "^\\*helm" "^\\*magit[^:]+:" "^\\*sdic\\*$"
               "^\\*vc\\(-.+\\)*\\*$" "^\\*xref\\*$")
             ibuffer-maybe-show-predicates))
   '(ibuffer-save-with-custom nil)))

(use-package imenu
  :bind (:map search-map ("i" . imenu))
  :config (add-hook 'imenu-after-jump-hook 'recenter))

(use-package isearch-dabbrev
  :bind (:map isearch-mode-map ("M-/" . isearch-dabbrev-expand)))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (custom-set-variables
   '(js-indent-level 2)
   '(js-switch-indent-offset 2)
   '(js2-idle-timer-delay 0.5)
   '(js2-include-jslint-globals nil)
   '(js2-include-jslint-declaration-externs nil))

  (add-to-list 'js2-mode-hook 'js2-imenu-extras-mode))

(use-package log-edit
  :defer t
  :config (custom-set-variables
           '(log-edit-hook '(log-edit-insert-filenames-without-changelog))))

(use-package ls-lisp
  :if (eq system-type 'windows-nt)
  :ensure nil
  :config (custom-set-variables '(ls-lisp-emulation 'MS-Windows)))

(use-package magit
  :bind (:map vc-prefix-map ("SPC" . magit-status))
  :config
  (custom-set-variables
   '(magit-log-margin '(t age-abbreviated magit-log-margin-width nil 18))
   '(magit-reflog-margin magit-log-margin)
   '(magit-log-section-commit-count 0))

  (add-hook 'git-commit-mode-hook
            (-partial 'set-buffer-file-coding-system 'utf-8-unix))

  (remove-hook 'server-switch-hook 'magit-commit-diff))

(use-package magit-find-file
  :bind (:map vc-prefix-map ("C-f" . magit-find-file-completing-read)))

(use-package man
  :defer t
  :config
  (custom-set-variables
   '(Man-heading-regexp "^\\([[:alpha:]][[:alnum:] /-]+\\)$")
   '(Man-see-also-regexp "SEE ALSO\\|関連項目")
   '(Man-first-heading-regexp
     "^NAME$\\|^名[前称]$\\|^[ \t]*No manual entry fo.*$")
   '(Man-synopsis-regexp "SYNOPSIS\\|書式")
   '(Man-files-regexp "FILES\\>\\|ファイル\\>"))

  (bind-keys :map Man-mode-map
             ("f" . scroll-up-command)
             ("b" . scroll-down-command)
             ("e" . scroll-up-line)
             ("y" . scroll-down-line)))

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

(use-package perspeen
  :config
  (custom-set-variables '(perspeen-mode t))

  (define-advice perspeen-update-mode-string
      (:override () display-only-current-ws)
    "Display only information of the current workspace on the modeline."
    (setq perspeen-modestring
          (list (nth 0 perspeen-modestring-dividers)
                (format "%d:%s"
                        (1+ (-find-index (-partial 'eq perspeen-current-ws)
                                         perspeen-ws-list))
                        (perspeen-ws-struct-name perspeen-current-ws))
                (nth 1 perspeen-modestring-dividers))))

  (define-advice perspeen-new-ws-internal
      (:after (&optional name) kill-new-scratch)
    "Kill the current buffer if it isn't equal the '*scratch*' buffer."
    (unless (eq (current-buffer) (get-buffer-create "*scratch*"))
      (kill-buffer))
    (switch-to-buffer "*scratch*"))

  (bind-key "C-z" 'perspeen-goto-last-ws perspeen-command-map))

(use-package python
  :defer t
  :config
  (custom-set-variables
   '(python-indent-guess-indent-offset nil)
   '(python-shell-completion-native-enable nil)))

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

  (when (package-installed-p 'helm)
    (custom-set-variables
     '(recentf-exclude
       (-union (-map (-compose 'regexp-quote
                               (-partial 'expand-file-name "../"))
                     (list data-directory package-user-dir))
               recentf-exclude)))

    (add-hook 'after-init-hook 'recentf-cleanup))

  (unless (package-installed-p 'helm)
    (bind-key "f" 'recentf-open-files mode-specific-map)))

(use-package rg
  :if (executable-find "rg")
  :defer t
  :config (custom-set-variables '(rg-custom-type-aliases nil)))

(use-package sdic
  :ensure nil
  :load-path (lambda () (expand-file-name "../sdic" package-user-dir))
  :bind (:map search-map ("k" . sdic-describe-word-at-point))
  :config
  (custom-set-variables
   '(sdic-default-coding-system 'utf-8-unix)
   '(sdicf-default-coding-system sdic-default-coding-system)
   '(sdic-eiwa-dictionary-list
     '((sdicf-client "~/.local/share/sdic/eedict.sdic" (strategy grep))
       (sdicf-client "~/.local/share/sdic/gene.sdic" (strategy grep))))
   '(sdic-waei-dictionary-list
     '((sdicf-client "~/.local/share/sdic/jedict.sdic" (strategy grep)))))

  (define-advice sdic-display-buffer
      (:override (&optional start-point) use-conditional-action)
    "`display-buffer-alist' で設定したアクションを使用するためのアドバイス。"
    (let ((cur-buf (current-buffer)))
      (unwind-protect
          (with-selected-window (display-buffer sdic-buffer-name)
            (goto-char (or start-point (point)))
            (set-window-start (selected-window) (point))
            (if (and sdic-warning-hidden-entry
                     (> (point) (point-min)))
                (message "この前にもエントリがあります。"))
            (buffer-size))
        (set-buffer cur-buf))))

  (when (executable-find "rg")
    (define-advice sdicf-grep-available (:override (sdic) file-check-only)
      (or (file-readable-p (sdicf-get-filename sdic))
          (signal 'sdicf-missing-file (list (sdicf-get-filename sdic)))))

    (define-advice sdicf-grep-search
        (:override (sdic pattern &optional case regexp) rg-search)
      "Search PATTERN from SDIC using rg."
      (sdicf-grep-init sdic)
      (with-current-buffer (sdicf-get-buffer sdic)
        (erase-buffer)
        (let ((default-process-coding-system
                (cons sdicf-default-coding-system
                      (cdr default-process-coding-system))))
          (apply 'call-process "rg" nil t nil
                 (append (if case '("-i"))
                         (if regexp '("-e") '("-F"))
                         (list pattern (sdicf-get-filename sdic)))))
        (goto-char (point-min))
        (let (entries)
          (while (not (eobp)) (sdicf-search-internal))
          (-uniq (-sort 'string< entries))))))

  (add-hook 'sdic-mode-hook
            (lambda () (bind-key "q" 'quit-window sdic-mode-map))))

(use-package shackle
  :config
  (custom-set-variables
   '(shackle-rules
     (-union '((ag-mode                :align t)
               (helm-major-mode        :align t)
               (occur-mode             :align t)
               (rg-mode                :align t)
               (sdic-mode              :align t :select t :ratio 0.3)
               (xref--xref-buffer-mode :align t :select t :ratio 0.3)
               ("*vc-log*"             :align t))
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
     '("a" "s" "d" "f" "j" "k" "l" "q" "w" "e" "r" "u" "i" "o" "p"))))

(use-package tern
  :if (executable-find "node")
  :hook (js-mode . tern-mode)
  :config
  (custom-set-variables '(tern-command '("tern" "--no-port-file")))

  (define-advice tern-mode (:around (fn &optional arg) executable-p)
    "Call `tern-mode' only when tern is executable."
    (if (executable-find "tern")
        (funcall fn arg))))

(use-package web-mode
  :mode ("\\.[sx]?html?\\'" . web-mode)
  :config
  (custom-set-variables
   '(web-mode-script-padding 2)
   '(web-mode-style-padding 2)
   '(web-mode-block-padding 2)
   '(web-mode-markup-indent-offset 2)
   '(web-mode-css-indent-offset 2)
   '(web-mode-code-indent-offset 2)
   '(web-mode-sql-indent-offset 2)
   '(web-mode-enable-css-colorization nil)
   '(web-mode-enable-whitespace-fontification t)
   '(web-mode-display-table nil))

  (custom-set-faces
   '(web-mode-whitespace-face ((t (:background "#eee8d5")))))

  (bind-key "C-c C-v" 'browse-url-of-buffer web-mode-map))

(use-package wgrep
  :after grep
  :config
  (custom-set-variables
   '(wgrep-enable-key "\C-c\C-e")
   '(wgrep-auto-save-buffer t)))

(use-package wgrep-ag
  :after ag)

(use-package wgrep-helm
  :after helm-regexp)

(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (custom-set-variables
   '(whitespace-style '(face tabs trailing))
   '(whitespace-trailing-regexp "\\([ \t\u00A0\u3000]+\\)$")
   '(whitespace-global-modes '(not web-mode))
   '(global-whitespace-mode t)))

(use-package winner
  :init (custom-set-variables '(winner-dont-bind-my-keys t))
  :config
  (custom-set-variables
   '(winner-boring-buffers
     (-union '("*sdic*" "*xref*") winner-boring-buffers))
   '(winner-mode t))

  (bind-keys :map ctl-x-map
             ("w"   . winner-undo)
             ("M-w" . winner-redo)))

(use-package woman
  :defer t
  :init (custom-set-variables '(woman-dired-keys nil))
  :config
  (custom-set-variables
   '(woman-manpath (list (expand-file-name "~/.local/share/man")))
   '(woman-cache-filename (expand-file-name "~/.cache/emacs/wmncache"))
   '(woman-imenu-generic-expression
     '((nil "\n\\([[:alpha:]].*\\)" 1)
       ("*Subsections*" "^   \\([[:alpha:]].*\\)" 1)))
   '(woman-fill-column 78)))

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
           :map minibuffer-local-map
           ("C-n" . next-line-or-history-element)
           ("C-p" . previous-line-or-history-element)
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
