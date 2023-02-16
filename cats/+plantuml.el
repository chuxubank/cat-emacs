;; -*- lexical-binding: t; -*-

(defconst plantuml-dark-arg "-darkmode")

(use-package plantuml-mode
  :defer t
  :custom
  (plantuml-default-exec-mode 'executable)
  (plantuml-indent-level 4)
  :config
  (defun plantuml-executable-start-process (buf)
    "Run PlantUML as an Emacs process and puts the output into the given buffer (as BUF)."
    (apply #'start-process
           "PLANTUML" buf plantuml-executable-path
           `(,@plantuml-executable-args
             ,(plantuml-jar-output-type-opt plantuml-output-type)
	     ,(if (+dark-mode-p) plantuml-dark-arg "")
             "-p"))))

(use-package flycheck-plantuml
  :after plantuml-mode
  :config
  (flycheck-plantuml-setup))

(with-eval-after-load 'ob-plantuml
  (setq org-plantuml-exec-mode 'plantuml))
