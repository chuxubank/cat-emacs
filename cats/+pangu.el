;; -*- lexical-binding: t; -*-

(use-package pangu-spacing
  :hook (text-mode . pangu-spacing-mode)
  :config
  (setq pangu-spacing-real-insert-separtor t)
  ;; Add `note-property'
  (defun pangu-spacing-org-mode-at-special-region ()
    (interactive)
    (let ((element (org-element-at-point)))
      (when (or (member (org-element-type element)
			'(src-block keyword example-block export-block
                                    latex-environment planning node-property))
		(member (car (org-element-context element))
			'(inline-src-block timestamp link code verbatim)))
	t))))

(defun +pangu-spacing-disable ()
  (pangu-spacing-mode -1))

(add-hook 'nxml-mode-hook #'+pangu-spacing-disable)
