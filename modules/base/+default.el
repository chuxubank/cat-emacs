;; -*- lexical-binding: t; -*-

;;; ui
(use-package frame
  :ensure nil
  :config
  (blink-cursor-mode 0))

(use-package face-remap
  :delight (buffer-face-mode (:eval (+with-icon "nf-md-format_font" " "))))

(use-package scroll-bar
  :demand t
  :ensure nil
  :config
  (scroll-bar-mode 0))

(use-package menu-bar
  :demand t
  :ensure nil
  :config
  (menu-bar-mode (if IS-ANDROID 1 0)))

(use-package tool-bar
  :demand t
  :ensure nil
  :config
  (tool-bar-mode 0))

(setq inhibit-startup-screen t
      initial-scratch-message nil
      frame-resize-pixelwise t
      frame-title-format '("%b %n - " cat-emacs-name)
      icon-title-format t)

(setq custom-safe-themes t)

(use-package icons
  :custom
  (icon-preference '(image emoji symbol text)))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

(use-package simple
  :ensure nil
  :delight
  (visual-line-mode
   (:eval (+with-icon (if word-wrap
                         "nf-cod-word_wrap"
                       "nf-md-wrap")
                     " ")))
  :hook
  (after-init . column-number-mode)
  (after-init . size-indication-mode)
  :custom
  (tab-width 4)
  (indent-tabs-mode nil)
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (completion-auto-select 'second-tab))

(setq show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(show-paren-mode 1)

(defun cat/show-trailing-whitespace ()
  "Set local variable `show-trailing-whitespace' to t."
  (setq show-trailing-whitespace t))

(defun cat/hide-trailing-whitespace ()
  "Set local variable `show-trailing-whitespace' to nil."
  (setq show-trailing-whitespace nil))

(add-hook 'text-mode-hook #'cat/show-trailing-whitespace)
(add-hook 'prog-mode-hook #'cat/show-trailing-whitespace)

;;; isearch
(setq isearch-lazy-count t)

(use-package flyspell
  :ensure nil
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :custom
  (flyspell-mode-line-string (+with-icon "nf-oct-typography" " ")))

;;; select
(delete-selection-mode 1)

;;; sound
(setq ring-bell-function #'ignore)

;;; buffer
(defalias 'list-buffers 'ibuffer)
(setq-default indicate-buffer-boundaries 'left)

(use-package url
  :ensure nil
  :custom
  (url-configuration-directory (concat cat-etc-dir "url/"))
  (url-automatic-caching t))

(use-package minibuffer
  :ensure nil
  :init
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  :custom
  (enable-recursive-minibuffers t)
  (completion-auto-help 'visible)
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)))

(savehist-mode)

;;; recentf
(setq recentf-max-saved-items 100)
(add-hook 'after-init-hook #'recentf-mode)

;;; saveplace
(add-hook 'after-init-hook #'save-place-mode)

;;; autosave
(setq auto-save-default t
      auto-save-include-big-deletions t)

(use-package ediff
  :ensure nil
  :init
  (defun cat/ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun cat/ediff-setup-keymap ()
    (define-key ediff-mode-map "d" 'cat/ediff-copy-both-to-C))
  :hook (ediff-keymap-setup . cat/ediff-setup-keymap)
  :custom
  (ediff-split-window-function #'split-window-horizontally))

;;; so long
(global-so-long-mode 1)

;;; repeat
(when EMACS28+
  (repeat-mode 1))

;;; help
(setq help-at-pt-display-when-idle t)

;;; scroll
(define-key ctl-x-4-map "v" 'view-file-other-window)
(define-key ctl-x-5-map "v" 'view-file-other-frame)

(use-package window
  :ensure nil
  :custom
  (fit-window-to-buffer-horizontally t))

;;; image
(setq image-use-external-converter t)

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-mode-text (+with-icon "nf-md-autorenew" " "))
  (auto-revert-tail-mode-text (+with-icon "nf-md-contain_end" " "))
  (global-auto-revert-ignore-modes '(logview-mode)))

(use-package files
  :ensure nil
  :custom
  (revert-buffer-quick-short-answers t)
  (confirm-kill-emacs #'yes-or-no-p))

(defconst cat-hs-folded-face
  '((t (:inherit 'font-lock-comment-face :box t)))
  "Face used to display the fold text.")

(defun cat/hs-folded-overlay-fn (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
           (info (format " ... #%d " nlines)))
      (overlay-put ov 'display (propertize info 'face cat-hs-folded-face)))))

(use-package hideshow
  :ensure nil
  :delight (hs-minor-mode
            (:eval (+with-icon "nf-md-arrow_collapse_vertical" " ")))
  :hook (prog-mode . hs-minor-mode)
  :custom
  (hs-set-up-overlay 'cat/hs-folded-overlay-fn))

(use-package type-break
  :ensure nil
  :hook
  (after-init . type-break-mode)
  (type-break-mode . type-break-query-mode)
  (type-break-mode . type-break-mode-line-message-mode)
  :custom
  (type-break-query-function #'y-or-n-p)
  (type-break-good-rest-interval (/ type-break-interval 10))
  (type-break-good-break-interval (* 5 60))
  (type-break-demo-boring-stats t)
  :config
  (type-break-guesstimate-keystroke-threshold 60))

(defun cat/type-break-buffer-p ()
  (string= (buffer-file-name) type-break-file-name))

(use-package epa-file
  :ensure nil
  :custom
  (epa-file-name-regexp "\\.\\(gpg\\|asc\\)\\(~\\|\\.~[0-9]+~\\)?\\'"))

(defun cat/epa-file-p ()
  (and buffer-file-name
       (bound-and-true-p epa-file-name-regexp)
       (string-match-p epa-file-name-regexp buffer-file-name)))

(use-package epg-config
  :ensure nil
  :custom
  (epg-pinentry-mode 'loopback))

(defun cat-clean-buffer-output (&optional beg end)
  "Clear buffer's ANSI color and control characters from BEG to END."
  (interactive "r")
  (save-excursion
    (let* ((beg (or beg (point-min)))
           (end (or end (point-max)))
           (text (buffer-substring-no-properties beg end)))
      (setq text (cat-handle-backspace text))
      (setq text (ansi-color-apply text))
      (let ((inhibit-read-only t))
        (delete-region beg end)
        (goto-char beg)
        (insert text)))))

(defun cat-handle-backspace (string)
  "Interpret backspaces (^H) in STRING like a terminal would."
  (let ((i 0)
        (res ""))
    (while (< i (length string))
      (let ((c (aref string i)))
        (if (and (= c ?\b) (> (length res) 0))
            (setq res (substring res 0 -1))
          (setq res (concat res (string c)))))
      (setq i (1+ i)))
    res))

(defun cat/colorize-after-shell-command-on-region (&rest _args)
  "Apply ANSI colors to the `shell-command-buffer-name' buffer and `minibuffer' after `shell-command-on-region'."
  (let ((bufs (seq-filter
               (lambda (x)
                 (or (string-prefix-p " *Echo Area" (buffer-name x))
                     (string-match-p shell-command-buffer-name (buffer-name x))))
               (buffer-list))))
    (dolist (buf bufs)
      (with-current-buffer buf
        (cat-clean-buffer-output)))))

(advice-add 'shell-command-on-region :after #'cat/colorize-after-shell-command-on-region)

(use-package ansi-color
  :ensure nil
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  (shell-mode . ansi-color-for-comint-mode-on))

(use-package compile
  :ensure nil
  :delight (compilation-shell-minor-mode
            (:eval (+with-icon "nf-cod-terminal_powershell" " "))))

(use-package profiler
  :ensure nil
  :init
  (defun cat/profiler-stop-and-report ()
    "Stop the profiler and show its report."
    (interactive)
    (profiler-report)
    (profiler-stop))
  :transient
  (cat-profiler
   (:description (+with-icon "nf-fa-tachometer" nil " Profiler"))
   ["Actions"
    ("s" "start" profiler-start :transient t)
    ("r" "stop & report" cat/profiler-stop-and-report)]))

(use-package midnight
  :ensure nil
  :hook
  (after-init . midnight-mode))

(use-package crm
  :ensure nil
  :config
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

(use-package xref
  :ensure nil
  :custom
  (xref-search-program 'ripgrep))

(use-package outline
  :ensure nil
  :delight (outline-minor-mode
            (:eval (+with-icon "nf-md-table_of_contents" " "))))

(defun cat/disable-electric-indent-chars ()
  "Set local variable `electric-indent-chars' to nil."
  (setq-local electric-indent-chars nil))

(use-package follow
  :ensure nil
  :custom
  (follow-mode-line-text (+with-icon "nf-cod-split_horizontal" " ")))

(use-package table
  :ensure nil
  :config
  (prosody-set-face-font 'table-cell 'table))

(use-package comp-run
  :ensure nil
  :custom
  (native-comp-async-report-warnings-errors 'silent))

(use-package warnings
  :ensure nil
  :custom
  (warning-minimum-level :error))

(use-package webjump
  :ensure nil
  :config
  (+add-to-list-multi 'webjump-sites
                      '("GitHub" .
                        [simple-query "github.com"
		                              "github.com/" ""])
                      '("GitHub Search" .
                        [simple-query "github.com"
		                              "github.com/search?q=" "&type=repositories"])))
