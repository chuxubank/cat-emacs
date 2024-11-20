;; -*- lexical-binding: t; -*-

(defun cat-plantuml-auto-theme ()
  "Adjust `plantuml-executable-args' and `org-plantuml-args' to align with Emacs' current theme."
  (let ((args (delq nil (list "-headless"
                              "-theme" (if (+dark-mode-p)
                                           "reddress-darkorange"
                                         "reddress-lightorange")))))
    (setq-default plantuml-executable-args args
                  org-plantuml-args args)))

(use-package plantuml-mode
  :mode ("\\.puml\\'" . plantuml-mode)
  :custom
  (plantuml-default-exec-mode 'executable)
  (plantuml-indent-level 4)
  :config
  (defun hex-encode (str)
    (string-join (mapcar (lambda (c) (format "%02x" c)) (string-as-unibyte str))))

  (defun plantuml-server-encode-url (string)
    "Encode the string STRING into a URL suitable for PlantUML server interactions."
    (let* ((encoded-string (hex-encode string)))
      (concat plantuml-server-url "/" plantuml-output-type "/~h" encoded-string)))

  (defun +plantuml-compute-url (&optional file)
    "Compute the PlantUML server URL for the current buffer or FILE."
    (interactive)
    (let* ((string (buffer-string))
           (in-file (or file (buffer-file-name)
                        (make-temp-file "plantuml-" nil ".puml"
                                        (if (string-prefix-p "@start" string t) string
                                          (format "@startuml\n%s\n@enduml" string)))))
           (data (string-trim (shell-command-to-string (format "%s -computeurl %s" plantuml-executable-path in-file))))
           (url (concat plantuml-server-url "/" plantuml-output-type "/" data)))
      (when (string-empty-p data) (error "Failed to compute URL"))
      (kill-new url)
      (message "Copied PlantUML server URL to kill ring")
      url))
  (add-hook 'cat-theme-refresh-hook #'cat-plantuml-auto-theme)
  (cat-plantuml-auto-theme))

(use-package flycheck-plantuml
  :demand t
  :after flycheck plantuml-mode
  :config
  (flycheck-define-checker plantuml
    "A checker using plantuml."
    :command ("plantuml" "-syntax")
    :standard-input t
    :error-patterns ((error line-start "ERROR" "\n" line "\n" (message) line-end))
    :modes plantuml-mode)
  (flycheck-plantuml-setup))

(with-eval-after-load 'ob-plantuml
  (setq org-plantuml-exec-mode 'plantuml))

(with-eval-after-load 'plantuml-mode
  (define-key plantuml-mode-map (kbd "C-c C-l") #'+plantuml-compute-url))
