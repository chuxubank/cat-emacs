(use-package nano-theme
  :straight (nano-theme :type git :host github :repo "rougier/nano-theme")
  :config
  (nano-setup)
  (nano-dark)
  (setq-default cursor-type 'box
		initial-major-mode 'lisp-interaction-mode))

(use-package nano-modeline
  :straight (nano-modeline :type git :host github :repo "rougier/nano-modeline")
  :config
  (nano-modeline))
