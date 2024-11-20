;; -*- lexical-binding: t; -*-

(defvar cat-mermaid-config-file (expand-file-name "mermaid/config.json" user-emacs-directory))

(defun cat-mermaid-theme ()
  (if (+dark-mode-p)
      "dark"
    "default"))

(use-package mermaid-mode
  :ensure-system-package
  (mmdc . "pnpm add -g @mermaid-js/mermaid-cli")
  :mode "\\.mmd\\'"
  :custom
  (mermaid-tmp-dir (concat cat-cache-dir "mermaid/"))
  (mermaid-output-format ".svg")
  (mermaid-flags (format "-b transparent -f -c %s -t %s" cat-mermaid-config-file (cat-mermaid-theme)))
  :config
  (mkdir mermaid-tmp-dir t))

(defun +mermaid-mode ()
  (setq-local indent-line-function 'insert-tab)
  (setq-local tab-width 4))

(add-hook 'mermaid-mode-hook #'+mermaid-mode)

(with-eval-after-load 'mermaid-mode
  (defun org-babel-execute:mermaid (body params)
    (let* ((out-file (or (cdr (assoc :file params))
                         (error "mermaid requires a \":file\" header argument")))
           (theme (cdr (assoc :theme params)))
           (width (cdr (assoc :width params)))
           (height (cdr (assoc :height params)))
           (background-color (cdr (assoc :background-color params)))
           (mermaid-config-file (cdr (assoc :mermaid-config-file params)))
           (css-file (cdr (assoc :css-file params)))
           (pupeteer-config-file (cdr (assoc :pupeteer-config-file params)))
           (temp-file (org-babel-temp-file "mermaid-"))
           (mmdc (or (executable-find mermaid-mmdc-location)
                     (error "`mermaid-mmdc-location' is not set and mmdc is not in `exec-path'")))
           (cmd (concat (shell-quote-argument (expand-file-name mmdc))
                        " -i " (org-babel-process-file-name temp-file)
                        " -o " (org-babel-process-file-name out-file)
                        (if theme
                            (concat " -t " theme)
                          (concat " -t " (cat-mermaid-theme)))
                        (when background-color
                          (concat " -b " background-color))
                        (when width
                          (concat " -w " (number-to-string width)))
                        (when height
                          (concat " -H " (number-to-string height)))
                        (if mermaid-config-file
                            (concat " -c " (org-babel-process-file-name mermaid-config-file))
                          (concat " -c " cat-mermaid-config-file))
                        (when css-file
                          (concat " -C " (org-babel-process-file-name css-file)))
                        (when pupeteer-config-file
                          (concat " -p " (org-babel-process-file-name pupeteer-config-file))))))
      (unless (file-executable-p mmdc)
        ;; cannot happen with `executable-find', so we complain about
        ;; `mermaid-mmdc-location'
        (error "Cannot find or execute %s, please check `mermaid-mmdc-location'" mmdc))
      (with-temp-file temp-file (insert body))
      (message "%s" cmd)
      (org-babel-eval cmd "")
      nil)))
