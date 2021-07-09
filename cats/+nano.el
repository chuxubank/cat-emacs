(use-package nano-theme
  :straight (nano-theme :type git :host github :repo "rougier/nano-theme")
  :config
  (setq nano-fonts-use t
	default-frame-alist (append (list '(internal-border-width . 24)
					  '(left-fringe . 0)
					  '(right-fringe . 0))))
  (nano-dark))

(use-package nano-modeline
  :straight (nano-modeline :type git :host github :repo "rougier/nano-modeline")
  :config
  (nano-modeline))
