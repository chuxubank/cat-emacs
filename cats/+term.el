;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-term
  (:color teal :title (+with-icon "nf-oct-terminal" "Term"))
  ("Term"
   (("t" #'term "term")
    ("a" #'ansi-term "ansi-term")
    ("S" #'serial-term "serial-term"))
   "Shell"
   (("s" #'shell "shell")
    ("e" #'eshell "eshell"))))

(use-package vterm
  :ensure-system-package
  (cmake . cmake)
  :bind
  (:map vterm-mode-map
        ("C-q" . #'vterm-send-next-key))
  (:map project-prefix-map
        ("V" . project-vterm))
  :pretty-hydra
  (cat-term
   ("Vterm"
    (("v" #'vterm "vterm")))))

(defun project-vterm ()
  (interactive)
  (defvar vterm-buffer-name)
  (let* ((default-directory (project-root (project-current t)))
         (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
         (vterm-buffer (get-buffer vterm-buffer-name)))
    (if (and vterm-buffer (not current-prefix-arg))
        (pop-to-buffer vterm-buffer  (bound-and-true-p display-comint-buffer-action))
      (vterm))))

(use-package mistty
  :bind
  (:map project-prefix-map
        ("M" . mistty-in-project))
  :pretty-hydra
  (cat-term
   ("Mistty"
    (("m" #'mistty "mistty")))))

(with-eval-after-load 'project
  (add-to-list 'project-switch-commands '(project-vterm "Vterm") t)
  (add-to-list 'project-switch-commands '(mistty-in-project "Mistty") t)
  (add-to-list 'project-kill-buffer-conditions '(major-mode . vterm-mode))
  (add-to-list 'project-kill-buffer-conditions '(major-mode . mistty-mode)))

(use-package eshell-vterm
  :hook (eshell-mode . eshell-vterm-mode))

(use-package vterm-toggle
  :bind
  (:map vterm-mode-map
        ([(control return)] . vterm-toggle-insert-cd)
        ("s-n" . vterm-toggle-forward)
        ("s-p" . vterm-toggle-backward))
  :pretty-hydra
  (cat-term
   ("Vterm"
    (("V" #'vterm-toggle "vterm toggle")))))

(when (package-installed-p 'meow)
  (use-package meow-vterm
    :vc (meow-vterm
         :url "https://github.com/accelbread/meow-vterm"
         :rev :newest)
    :demand t
    :after vterm meow
    :config
    (meow-vterm-enable)))
