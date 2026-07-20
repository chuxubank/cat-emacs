;; -*- lexical-binding: t; -*-

(use-package ansible
  :delight)

(use-package ansible-doc
  :delight)

(defun cat/ansible-template-mode-setup ()
  "Enable Ansible helpers in a Jinja2 polymode host buffer."
  (when (and buffer-file-name
             (or (string-match-p "/ansible/.*\\.ya?ml\\'"
                                 buffer-file-name)
                 (string-match-p "/\\(?:group\\|host\\)_vars/"
                                 buffer-file-name)))
    (ansible-mode 1)
    (ansible-doc-mode 1)))

(use-package poly-any-jinja2
  :after ansible ansible-doc
  :init
  (setq poly-any-jinja2-extra-file-name-rules
        '("/ansible/.*\\.ya?ml\\'"
          "/\\(?:group\\|host\\)_vars/"))
  :hook
  (poly-any-template-after-activate . cat/ansible-template-mode-setup))

(use-package flymake-ansible-lint
  :hook
  (ansible-mode . flymake-ansible-lint-setup)
  (ansible-mode . flymake-mode))
