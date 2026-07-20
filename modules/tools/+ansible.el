;; -*- lexical-binding: t; -*-

(use-package ansible
  :delight)

(use-package ansible-doc
  :delight)

(defun cat/ansible-vars-file-p (filename)
  "Return non-nil when FILENAME is below group_vars or host_vars."
  (and filename
       (string-match-p "/\\(?:group\\|host\\)_vars/" filename)))

(defun cat/ansible-template-file-p (filename)
  "Return non-nil when FILENAME is an Ansible template source."
  (and filename
       (or (string-match-p "/ansible/.*\\.ya?ml\\'" filename)
           (cat/ansible-vars-file-p filename))))

(defun cat/ansible-template-host-filename (filename)
  "Give extensionless Ansible vars FILENAME a YAML host suffix."
  (if (and (cat/ansible-vars-file-p filename)
           (not (file-name-extension filename)))
      (concat filename ".yaml")
    filename))

(defun cat/ansible-template-mode-setup ()
  "Enable Ansible helpers in a Jinja2 polymode host buffer."
  (when (and buffer-file-name
             (cat/ansible-template-file-p buffer-file-name))
    (ansible-mode 1)
    (ansible-doc-mode 1)))

(use-package poly-any-jinja2
  :init
  (setq poly-any-jinja2-extra-file-name-rules
        '(cat/ansible-template-file-p))
  (add-hook 'poly-any-template-host-filename-functions
            #'cat/ansible-template-host-filename)
  :hook
  (poly-any-template-after-activate . cat/ansible-template-mode-setup))

(use-package flymake-ansible-lint
  :hook
  (ansible-mode . flymake-ansible-lint-setup)
  (ansible-mode . flymake-mode))
