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
             "-p")))

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
      url)))

(use-package flycheck-plantuml
  :after plantuml-mode
  :config
  (flycheck-plantuml-setup))

(with-eval-after-load 'ob-plantuml
  (setq org-plantuml-exec-mode 'plantuml))

(with-eval-after-load 'plantuml-mode
  (define-key plantuml-mode-map (kbd "C-c C-l") #'+plantuml-compute-url))
