;; -*- lexical-binding: t; -*-

(use-package embark
  :bind
  ("C->" . embark-act)
  ("C-M->" . embark-dwim)
  ("C-h B" . embark-bindings)
  (:map embark-general-map
        ("G" . +embark-google-search))
  (:map embark-variable-map
        (":" . +embark-act-with-eval))
  (:map embark-expression-map
        (":" . +embark-act-with-eval))
  (:map embark-region-map
        ("!" . +embark-shell)
        ("&" . +embark-async-shell))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (defun +embark-google-search (term)
    (interactive "sSearch Term: ")
    (browse-url
     (format "http://google.com/search?q=%s" term)))
  (defun +embark-act-with-eval (expression)
    "Evaluate EXPRESSION and call `embark-act' on the result."
    (interactive "sExpression: ")
    (with-temp-buffer
      (insert (eval (read expression)))
      (embark-act)))
  (defun +embark-shell (term)
    (shell-command term))
  (defun +embark-async-shell (term)
    (async-shell-command term))
  (when (package-installed-p 'password-store)
    (defvar-keymap embark-password-store-actions
      :doc "Keymap for actions for password-store."
      "c" #'password-store-copy
      "f" #'password-store-copy-field
      "i" #'password-store-insert
      "I" #'password-store-generate
      "r" #'password-store-rename
      "e" #'password-store-edit
      "k" #'password-store-remove
      "U" #'password-store-url)

    (add-to-list 'embark-keymap-alist '(password-store . embark-password-store-actions))
    (add-to-list 'marginalia-prompt-categories '("Password entry" . password-store))))

(when (package-installed-p 'consult)
  (use-package embark-consult
    :hook
    (embark-collect-mode . consult-preview-at-point-mode)))

(when (package-installed-p 'avy)
  (use-package avy-embark-collect
    :after embark
    :bind
    (:map embark-collect-mode-map
          ("C-'" . avy-embark-collect-choose)
          ("C-\"" . avy-embark-collect-act))))

(when (package-installed-p 'which-key)
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))
