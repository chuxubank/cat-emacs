;; -*- lexical-binding: t; -*-

(use-package compile-multi
  :custom
  (compile-multi-default-directory
   (lambda ()
     (project-root (project-current t))))
  :bind
  (:map project-prefix-map
        ("C-c" . #'compile-multi))
  :pretty-hydra
  (cat-workspace
   ("Project"
    (("c" #'compile-multi "compile multi"))))
  :config
  (push `((file-exists-p "Makefile")
          ,#'compile-multi-make-targets+)
        compile-multi-config))

(defun compile-multi-make-targets+ ()
  (when-let* ((dir (locate-dominating-file default-directory "Makefile"))
              (makefile (expand-file-name "Makefile" dir))
              (default-directory dir))
    (require 'make-mode)
    (with-temp-buffer
      (insert-file-contents makefile)
      (makefile-mode)
      (setq-local makefile-need-target-pickup t)
      (makefile-pickup-targets)
      (cl-loop for (target) in makefile-target-table
               unless (string-prefix-p "." target)
               collect (cons (format "make:%s" target)
                             (format "make %s" target))))))

(use-package consult-compile-multi
  :demand t
  :after compile-multi
  :config
  (consult-compile-multi-mode))

(use-package compile-multi-embark
  :demand t
  :after embark compile-multi
  :config
  (compile-multi-embark-mode +1))

(use-package compile-multi-nerd-icons
  :demand t
  :after nerd-icons-completion compile-multi)
