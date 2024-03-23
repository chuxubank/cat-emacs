;; -*- lexical-binding: nil; -*-

;;; os
(defconst EMACS28+   (> emacs-major-version 27))
(defconst EMACS29+   (> emacs-major-version 28))
(defconst EMACS30+   (> emacs-major-version 29))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-MACPORT (functionp 'mac-application-state))
(defconst IS-MACPLUS (boundp 'ns-system-appearance))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-MINGW64 (and IS-WINDOWS (string-match "mingw64" (getenv "emacs_dir"))))
(defconst IS-WSL     (string-match-p "WSL2" operating-system-release))

(defconst cat-emacs-name "Cat Emacs")

;;; directory
(defconst cat-local-dir (concat user-emacs-directory ".local/"))
(defconst cat-cache-dir (concat cat-local-dir "cache/"))
(defconst cat-etc-dir (concat cat-local-dir "etc/"))

(defun +mkdir-p (dir)
  "Make directory for DIR if not exists."
  (unless (file-directory-p dir)
    (make-directory dir)))

(+mkdir-p cat-local-dir)
(+mkdir-p cat-cache-dir)
(+mkdir-p cat-etc-dir)

(defconst cat-rime-dir (concat cat-etc-dir "rime/"))
(defconst cat-org-directory "~/org/"
  "See `org-directory'.")
(defconst cat-org-roam-directory "~/org-roam/"
  "See `org-roam-directory'.")
(defconst cat-org-roam-dailies-directory "daily/"
  "See `org-roam-dailies-directory'.")
(defconst cat-org-roam-reference-directory "reference/"
  "Orb notes directory.
Used by `citar-notes-paths' and `org-roam-capture-templates'.")
(defconst cat-default-bibliography-files '("~/Zotero/My Library.bib"))
(defconst cat-default-csl-styles-dir "~/Zotero/styles/")
(defconst cat-codeium-dir "~/.codeium/")
(defconst cat-codeium-api-key-file (expand-file-name "api_key" cat-codeium-dir))

;;; benchmark
(defun cat-benchmark (pos &optional file)
  "Print the current time of load POS of FILE."
  (message "%s %s of %s"
           (format-time-string "%T %3N")
           (upcase (symbol-name pos))
           (or file load-file-name buffer-file-name)))

;;; modifier-key
(cond
 (IS-MACPORT
  (setq mac-command-modifier 'meta
        mac-option-modifier 'meta
        mac-right-command-modifier 'super
        mac-right-option-modifier 'none))
 (IS-MAC
  (setq ns-command-modifier 'meta
        ns-option-modifier 'meta
        ns-right-command-modifier 'super
        ns-right-option-modifier 'none))
 (IS-WINDOWS
  (setq w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super)))

(cat-benchmark 'end)
