;;; cat-ansible-test.el --- Tests for Cat Ansible integration -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(setq use-package-always-defer t
      use-package-always-ensure nil)

(load (expand-file-name "modules/tools/+ansible.el" user-emacs-directory)
      nil t)

(ert-deftest cat-ansible-direct-open-activates-template-support ()
  "Directly opening an Ansible template activates its composed mode."
  (dolist (filename '("/tmp/ansible/playbook.yaml"
                      "/tmp/inventory/group_vars/all"))
    (with-temp-buffer
      (setq buffer-file-name filename)
      (normal-mode t)
      (should (derived-mode-p 'yaml-mode))
      (should (bound-and-true-p polymode-mode))
      (should (bound-and-true-p ansible-mode))
      (should (bound-and-true-p ansible-doc-mode)))))

(provide 'cat-ansible-test)
;;; cat-ansible-test.el ends here
