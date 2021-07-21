(use-package pangu-spacing
  :hook (text-mode . pangu-spacing-mode)
  :config
  (defun +pangu-temp-insert-real ()
    (set (make-local-variable 'pangu-spacing-real-insert-separtor) t))
  (add-hook 'org-mode-hook #'+pangu-temp-insert-real))
