;;; os
(defconst EMACS27+   (> emacs-major-version 26))
(defconst EMACS28+   (> emacs-major-version 27))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-MACPORT (functionp 'mac-application-state))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-MINGW64 (and IS-WINDOWS (string-match "mingw64" (getenv "emacs_dir"))))
(defconst IS-WSL     (string-match-p "WSL2" operating-system-release))

;;; directory
(defconst cat-local-dir (concat user-emacs-directory ".local/"))
(defconst cat-cache-dir (concat cat-local-dir "cache/"))
(defconst cat-etc-dir (concat cat-local-dir "etc/"))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

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
