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

;;; ui
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;;; frame
(setq frame-title-format '("%b – Cat Emacs")
      icon-title-format frame-title-format)

;;; minibuffer
(fset #'yes-or-no-p #'y-or-n-p)
