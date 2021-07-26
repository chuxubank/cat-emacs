(use-package nano-theme
  :straight (nano-theme :type git :host github :repo "rougier/nano-theme")
  :init
  (defun is-dark-mode()
    (string-equal "0" (string-trim (shell-command-to-string "powershell -C Get-ItemPropertyValue -Path HKCU://Software/Microsoft/Windows/CurrentVersion/Themes/Personalize -Name AppsUseLightTheme"))))
  :config
  (setq
   default-frame-alist (append (list '(internal-border-width . 24)
				     '(left-fringe . 0)
				     '(right-fringe . 0)
				     '(vertical-scroll-bars . nil)
				     '(fullscreen . maximized))))
  ;; Nicer glyphs for continuation and wrap
  (set-display-table-slot standard-display-table
			  'truncation (make-glyph-code ?… 'nano-faded))
  (set-display-table-slot standard-display-table
			  'wrap (make-glyph-code ?… 'nano-faded))

  (if IS-WINDOWS
      (add-hook 'after-init-hook (lambda ()
				   (if (is-dark-mode)
				       (nano-dark)
				     (nano-light))))
    (nano-dark)))

(use-package nano-modeline
  :straight (nano-modeline :type git :host github :repo "rougier/nano-modeline")
  :config
  (nano-modeline)
  (advice-add #'nano-dark :after #'nano-modeline)
  (advice-add #'nano-light :after #'nano-modeline))
