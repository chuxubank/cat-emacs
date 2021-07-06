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
