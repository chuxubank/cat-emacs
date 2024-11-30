;; -*- lexical-binding: t; -*-

(use-package treemacs
  :bind
  ("M-0" . treemacs-select-window)
  :custom
  (treemacs-is-never-other-window t))

(use-package treemacs-magit
  :demand t
  :after treemacs magit)

(use-package treemacs-tab-bar
  :demand t
  :after treemacs
  :config
  (treemacs-set-scope-type 'Tabs))

(use-package treemacs-nerd-icons
  :demand t
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(with-eval-after-load 'treemacs
  (with-eval-after-load 'activities
    (add-hook 'activities-anti-save-predicates #'treemacs-is-treemacs-window-selected?)
    (add-hook 'activities-after-resume-functions #'+treemacs-correct-in-this-buffer)))

(defun +treemacs-correct-in-this-buffer (_)
  "Select the `treemacs' buffer after restore."
  (pcase (treemacs-current-visibility)
    ('visible (setq-local treemacs--in-this-buffer t))))
