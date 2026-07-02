;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-term
  (:color teal :title (+with-icon "nf-oct-terminal" "Term"))
  ("" ()))

(use-package term
  :ensure nil
  :pretty-hydra
  (cat-term
   ("Term"
    (("t" #'term "term")
     ("a" #'ansi-term "ansi-term")
     ("S" #'serial-term "serial-term")))))

(use-package shell
  :ensure nil
  :pretty-hydra
  (cat-term
   ("Shell"
    (("s" #'shell "shell")))))

(use-package eshell
  :ensure nil
  :pretty-hydra
  (cat-term
   ("Shell"
    (("e" #'eshell "eshell")))))

(use-package vterm
  :cat
  :ensure-system-package
  (cmake . cmake)
  :init
  (defun project-vterm ()
    (interactive)
    (defvar vterm-buffer-name)
    (let* ((default-directory (project-root (project-current t)))
           (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
           (vterm-buffer (get-buffer vterm-buffer-name)))
      (if (and vterm-buffer (not current-prefix-arg))
          (pop-to-buffer vterm-buffer  (bound-and-true-p display-comint-buffer-action))
        (vterm))))
  :bind
  (:map vterm-mode-map
        ("C-q" . #'vterm-send-next-key))
  (:map project-prefix-map
        ("V" . project-vterm))
  :pretty-hydra
  (cat-term
   ("Vterm"
    (("v" #'vterm "vterm")))))

(use-package mistty
  :bind
  (:map project-prefix-map
        ("M" . mistty-in-project))
  :pretty-hydra
  (cat-term
   ("Mistty"
    (("m" #'mistty "mistty")))))

(with-eval-after-load 'project
  (when (catp! vterm)
    (add-to-list 'project-switch-commands '(project-vterm "Vterm") t)
    (add-to-list 'project-kill-buffer-conditions '(major-mode . vterm-mode)))
  (add-to-list 'project-switch-commands '(mistty-in-project "Mistty") t)
  (add-to-list 'project-kill-buffer-conditions '(major-mode . mistty-mode)))

(use-package eshell-vterm
  :cat vterm
  :hook (eshell-mode . eshell-vterm-mode))

(use-package vterm-toggle
  :cat vterm
  :bind
  (:map vterm-mode-map
        ([(control return)] . vterm-toggle-insert-cd)
        ("s-n" . vterm-toggle-forward)
        ("s-p" . vterm-toggle-backward))
  :pretty-hydra
  (cat-term
   ("Vterm"
    (("V" #'vterm-toggle "vterm toggle")))))

(use-package meow-vterm
  :cat (and (catp! vterm) (modulep! meow))
  :vc (:url "https://github.com/accelbread/meow-vterm")
  :demand t
  :after vterm meow
  :config
  (meow-vterm-enable))

(use-package ghostel
  :pretty-hydra
  (cat-term
   ("Ghostty"
    (("g" #'ghostel "Ghostel")))))

(use-package kitty-graphics
  :vc (:url "https://github.com/cashmeredev/kitty-graphics.el")
  :delight " 󰘒"
  :when (not (display-graphic-p))
  :custom
  (kitty-gfx-enable-video t)
  :config
  (kitty-graphics-mode 1))

(use-package shell-maker
  :custom
  (shell-maker-root-path (concat cat-local-dir "shell-maker/"))
  :config
  (+mkdir-p shell-maker-root-path))
