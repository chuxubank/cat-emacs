;; -*- lexical-binding: t; -*-

;;; ui
(blink-cursor-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq inhibit-startup-screen t
      initial-scratch-message nil
      frame-resize-pixelwise t)

(setq frame-title-format '("%b – " cat-emacs-name)
      icon-title-format frame-title-format)

(setq custom-safe-themes t)

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

(use-package simple
  :ensure nil
  :delight
  (visual-line-mode (:eval (if word-wrap " " " 󰖶")))
  :hook
  (after-init . column-number-mode)
  (after-init . size-indication-mode)
  :custom
  (tab-width 4)
  (indent-tabs-mode nil)
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p))

(setq show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(show-paren-mode 1)

(defun cat-show-trailing-whitespace ()
  "Set local variable `show-trailing-whitespace' to t."
  (setq show-trailing-whitespace t))

(defun cat-hide-trailing-whitespace ()
  "Set local variable `show-trailing-whitespace' to nil."
  (setq show-trailing-whitespace nil))

(add-hook 'text-mode-hook #'cat-show-trailing-whitespace)
(add-hook 'prog-mode-hook #'cat-show-trailing-whitespace)

;;; isearch
(setq isearch-lazy-count t)

(use-package flyspell
  :ensure nil
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :custom
  (flyspell-mode-line-string " "))

;;; select
(delete-selection-mode 1)

;;; sound
(setq ring-bell-function #'ignore)

;;; buffer
(defalias 'list-buffers 'ibuffer)

;;; url
(setq url-configuration-directory (concat cat-etc-dir "url/"))

;;; bookmarks
(setq bookmark-default-file (expand-file-name "bookmarks" cat-etc-dir))

;;; minibuffer
(setq enable-recursive-minibuffers t
      confirm-kill-emacs #'yes-or-no-p)

(setq savehist-file (expand-file-name "minibuffer/history" cat-cache-dir))
(savehist-mode)

;;; eshell
(setq eshell-history-file-name (expand-file-name "eshell/history" cat-cache-dir))

;;; recentf
(setq recentf-max-saved-items 100
      recentf-save-file (expand-file-name "recentf" cat-cache-dir))
(add-hook 'after-init-hook #'recentf-mode)

;;; saveplace
(setq save-place-file (expand-file-name "places" cat-cache-dir))
(add-hook 'after-init-hook #'save-place-mode)

;;; backup
(setq create-lockfiles nil
      make-backup-files nil
      backup-directory-alist (list (cons "." (concat cat-cache-dir "backup/")))
      tramp-backup-directory-alist backup-directory-alist)

;;; autosave
(setq auto-save-default t
      auto-save-include-big-deletions t
      auto-save-list-file-prefix (concat cat-cache-dir "autosave/")
      tramp-auto-save-directory  (concat cat-cache-dir "tramp-autosave/")
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

;;; ediff
(setq ediff-diff-options "-w" ; turn off whitespace checking
      ediff-split-window-function #'split-window-horizontally)
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))

(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

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

;;; gnus
(setq gnus-select-method '(nntp "news.gmane.io"))

;;; image
(setq image-use-external-converter t)

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-mode-text " 󰁪"))

(use-package files
  :ensure nil
  :custom
  (revert-buffer-quick-short-answers t))

(use-package hideshow
  :ensure nil
  :delight (hs-minor-mode " 󰡍")
  :hook (prog-mode . hs-minor-mode)
  :init
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))
  (defun hideshow-folded-overlay-fn (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " ... #%d " nlines)))
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))
  :custom
  (hs-set-up-overlay 'hideshow-folded-overlay-fn))
;;; pcache
(setq pcache-directory (concat cat-cache-dir "pcache/"))

(use-package type-break
  :ensure nil
  :hook
  (after-init . type-break-mode)
  (type-break-mode . type-break-query-mode)
  (type-break-mode . type-break-mode-line-message-mode)
  :custom
  (type-break-file-name (concat cat-etc-dir "type-break"))
  (type-break-query-function #'y-or-n-p)
  :config
  (type-break-guesstimate-keystroke-threshold 30))

;;; time
(setq timeclock-file (concat cat-etc-dir "timelog"))

(use-package epg-config
  :ensure nil
  :custom
  (epg-pinentry-mode 'loopback))

(use-package abbrev
  :ensure nil
  :custom
  (abbrev-file-name (concat cat-etc-dir "abbrev_defs")))

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package compile
  :ensure nil
  :delight (compilation-shell-minor-mode " "))

(use-package transient
  :pin gnu
  :custom
  (transient-levels-file (concat cat-etc-dir "transient/levels"))
  (transient-values-file (concat cat-etc-dir "transient/values"))
  (transient-history-file (concat cat-etc-dir "transient/history")))

(use-package profiler
  :ensure nil
  :pretty-hydra
  ((:color pink :quit-key "q")
   ("Profiler"
    (("s" profiler-start "start")
     ("r" (progn (profiler-report) (profiler-stop)) "stop & report" :exit t)))))
