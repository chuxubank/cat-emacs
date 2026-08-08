;; -*- lexical-binding: t; -*-

(prosody-register
 'comint
 '(:modes comint-mode
   :font terminal
   :rescale (("Symbols Nerd Font" . 1.2))))

(mode-transient-define-prefix cat-term ()
  :description (+with-icon "nf-oct-terminal" nil " Term"))

(use-package term
  :ensure nil
  :transient
  (cat-term
   ["Term"
    ("t" "term" term)
    ("a" "ansi-term" ansi-term)
    ("S" "serial-term" serial-term)]))

(use-package shell
  :ensure nil
  :transient
  (cat-term
   ["Shell"
    ("s" "shell" shell)]))

(use-package eshell
  :ensure nil
  :transient
  (cat-term
   ["Shell"
    ("e" "eshell" eshell)]))

(use-package vterm
  :cat
  :font-role (terminal
             :rescale (("Symbols Nerd Font" . 1.2)))
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
  :transient
  (cat-term
   ["Vterm"
    ("v" "vterm" vterm)]))

(use-package mistty
  :font-role (terminal
             :rescale (("Symbols Nerd Font" . 1.2)))
  :bind
  (:map project-prefix-map
        ("M" . mistty-in-project))
  :transient
  (cat-term
   ["Mistty"
    ("m" "mistty" mistty)]))

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
  :transient
  (cat-term
   ["Vterm"
    ("V" "vterm toggle" vterm-toggle)]))

(use-package meow-vterm
  :cat (and (catp! vterm) (modulep! meow))
  :vc (:url "https://github.com/accelbread/meow-vterm")
  :demand t
  :after vterm meow
  :config
  (meow-vterm-enable))

(use-package ghostel
  :font-role (terminal
             :rescale (("Symbols Nerd Font" . 1.2)))
  :transient
  (cat-term
   ["Ghostty"
    ("g" "Ghostel" ghostel)]))

(use-package kitty-graphics
  :vc (:url "https://github.com/cashmeredev/kitty-graphics.el")
  :delight (kitty-graphics-mode
            (:eval (+with-icon "nf-md-watermark" " ")))
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
