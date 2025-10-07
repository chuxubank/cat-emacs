;; -*- lexical-binding: t; -*-

(use-package consult
  :bind
  ([remap Info-search] . consult-info)
  ([remap yank-pop] . consult-yank-pop)
  ([remap goto-line] . consult-goto-line)
  ([remap recentf-open-files] . consult-recent-file)
  ([remap bookmark-jump] . consult-bookmark)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap project-switch-to-buffer] . consult-project-buffer)
  ([remap repeat-complex-command] . consult-complex-command)
  ([remap isearch-edit-string] . consult-isearch-history)
  (:map mode-specific-map
        ("M-x" . consult-mode-command)
        ("h" . consult-history)
        ("k" . consult-kmacro)
        ("m" . consult-man)
        ("i" . consult-info))
  (:map goto-map
        ("e" . consult-compile-error)
        ("f" . consult-flymake)
        ("o" . consult-outline)
        ("m" . consult-mark)
        ("k" . consult-global-mark)
        ("i" . consult-imenu)
        ("I" . consult-imenu-multi))
  (:map search-map
        ("d" . consult-find)
        ("D" . consult-locate)
        ("f" . consult-fd)
        ("g" . consult-grep)
        ("G" . consult-git-grep)
        ("r" . consult-ripgrep)
        ("l" . consult-line)
        ("L" . consult-line-multi)
        ("k" . consult-keep-lines)
        ("u" . consult-focus-lines)
        ("e" . consult-isearch-history))
  (:map isearch-mode-map
        ("M-s l" . consult-line)              ;; needed by consult-line to detect isearch
        ("M-s L" . consult-line-multi))       ;; needed by consult-line to detect isearch
  (:map minibuffer-local-map
        ("M-s" . consult-history)             ;; orig. next-matching-history-element
        ("M-r" . consult-history))            ;; orig. previous-matching-history-element
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (advice-add #'register-preview :override #'consult-register-window)
  :custom
  (consult-narrow-key "<")
  (consult-async-min-input 2)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  :mode-hydra
  (org-mode
   ("Plugin"
    (("h" consult-org-heading "headings")))))
