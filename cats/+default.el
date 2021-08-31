;;; os
(defconst EMACS27+   (> emacs-major-version 26))
(defconst EMACS28+   (> emacs-major-version 27))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-MINGW64 (and IS-WINDOWS (string-match "mingw64" (getenv "emacs_dir"))))
(defconst IS-WSL1    (and IS-LINUX (string-match "-Microsoft" operating-system-release)))
(defconst IS-WSL2    (and IS-LINUX (string-match "-microsoft" operating-system-release)))
(defconst IS-WSL     (or IS-WSL1 IS-WSL2))

;;; directory
(defconst cat-local-dir (concat user-emacs-directory ".local/"))
(defconst cat-cache-dir (concat cat-local-dir "cache/"))
(defconst cat-etc-dir (concat cat-local-dir "etc/"))

(setq find-function-C-source-directory (format "%s/share/emacs/%s/src/" (getenv "emacs_dir") emacs-version))

;;; ui
(blink-cursor-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq inhibit-startup-screen t
      initial-scratch-message nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq frame-title-format '("%b – Cat Emacs")
      icon-title-format frame-title-format)

(setq custom-safe-themes t)

(setq column-number-indicator-zero-based nil)
(column-number-mode 1)

(global-visual-line-mode 1)

;;; sound
(setq ring-bell-function #'ignore)

;;; buffer
(defalias 'list-buffers 'ibuffer)

;;; minibuffer
(setq enable-recursive-minibuffers t)
(setq confirm-kill-emacs #'yes-or-no-p)
(fset #'yes-or-no-p #'y-or-n-p)

;;; coding
(set-language-environment "UTF-8")

;;; recentf
(setq recentf-max-saved-items 100)
(recentf-mode 1)

;;; backup
(setq create-lockfiles nil
      make-backup-files nil)

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

;;; dired
(setq dired-dwim-target t)

(let ((args (list "-ahlv" "--group-directories-first")))
  (when IS-BSD
    ;; Use GNU ls as `gls' from `coreutils' if available. Add `(setq
    ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning
    ;; when not using GNU ls.
    (if-let (gls (executable-find "gls"))
        (setq insert-directory-program gls)
      ;; BSD ls doesn't support --group-directories-first
      (setq args (list (car args)))))
  (setq dired-listing-switches (string-join args " ")))
