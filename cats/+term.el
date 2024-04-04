;; -*- lexical-binding: t; -*-

(use-package vterm
  :ensure-system-package
  (cmake . cmake)
  :bind
  (:map vterm-mode-map
        ("C-q" . #'vterm-send-next-key))
  (:map project-prefix-map
        ("V" . project-vterm)))

(defun project-vterm ()
  (interactive)
  (defvar vterm-buffer-name)
  (let* ((default-directory (project-root (project-current t)))
         (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
         (vterm-buffer (get-buffer vterm-buffer-name)))
    (if (and vterm-buffer (not current-prefix-arg))
        (pop-to-buffer vterm-buffer  (bound-and-true-p display-comint-buffer-action))
      (vterm))))

(with-eval-after-load 'project
  (add-to-list 'project-switch-commands '(project-vterm "Vterm") t)
  (add-to-list 'project-kill-buffer-conditions '(major-mode . vterm-mode)))

(use-package eshell-vterm
  :hook (eshell-mode . eshell-vterm-mode))

(use-package vterm-toggle
  :bind
  (:map vterm-mode-map
        ([(control return)] . vterm-toggle-insert-cd)
        ("s-n" . vterm-toggle-forward)
        ("s-p" . vterm-toggle-backward)))

(when (package-installed-p 'meow)
  (use-package meow-vterm
    :vc (meow-vterm
         :url "https://github.com/accelbread/meow-vterm"
         :rev :newest)
    :demand t
    :after vterm meow
    :config
    (meow-vterm-enable)))
