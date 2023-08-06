;; -*- lexical-binding: t; -*-

(use-package rime
  :commands #'toggle-input-method)

(when IS-MAC
  (setq rime-librime-root (concat cat-rime-dir "dist/"))
  (defun +rime-librime-download-install ()
    "Download and install the librime from GitHub"
    (interactive)
    (let* ((url "https://api.github.com/repos/rime/librime/releases/latest")
           (download-dir (expand-file-name "~/Downloads"))
           (download-url-cmd (concat "curl -s " url " | jq -r '.assets[] | select(.name | contains(\"macOS\") and (test(\"deps\") | not)) | .browser_download_url'"))
           (download-url (string-trim-right (shell-command-to-string download-url-cmd)))
           (downloaded-file (concat download-dir "/" (file-name-nondirectory download-url)))
           (log-buffer-name "*librime-download-log*")
           (version-file (concat rime-librime-root "version-info.txt")))
      (when (file-exists-p rime-librime-root)
        (delete-directory rime-librime-root t))
      (when (file-exists-p version-file)
        (delete-file version-file t))
      (with-current-buffer (get-buffer-create log-buffer-name)
        (erase-buffer)
        (insert (format "Downloading from: %s\n" download-url)))
      (let ((download-command (format "curl -L -o %s %s" downloaded-file download-url))
            (extract-command (format "tar -xjf %s -C %s" downloaded-file cat-rime-dir)))
        (if (file-exists-p downloaded-file)
            (progn
              (shell-command extract-command)
              (message "Latest librime for macOS has been extracted to %s\n" rime-librime-root))
          (+start-process-with-finish-callback
           "download-librime"
           log-buffer-name
           download-command
           (lambda ()
             (shell-command extract-command)
             (message "Latest librime for macOS has been downloaded and extracted to %s\n" rime-librime-root))))))))

(setq
 rime-user-data-dir (concat cat-rime-dir "data/")
 rime-disable-predicates
 '(rime-predicate-hydra-p
   rime-predicate-ace-window-p
   rime-predicate-prog-in-code-p
   rime-predicate-org-latex-mode-p
   rime-predicate-org-in-src-block-p
   rime-predicate-after-ascii-char-p
   rime-predicate-tex-math-or-command-p
   rime-predicate-punctuation-line-begin-p
   rime-predicate-punctuation-after-space-cc-p)
 rime-inline-predicates
 '(rime-predicate-space-after-cc-p
   rime-predicate-current-uppercase-letter-p)
 rime-translate-keybindings
 '("<C-delete>" "C-f" "C-b" "C-n" "C-p" "C-g")
 rime-show-candidate 'posframe
 default-input-method "rime"
 rime-cursor "|")

(with-eval-after-load 'rime
  (define-key rime-active-mode-map [tab] 'rime-inline-ascii)
  (define-key rime-mode-map (kbd "C-`") 'rime-send-keybinding)
  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable))

(when (featurep 'meow)
  (+add-to-list-multi 'rime-disable-predicates
		      #'meow-normal-mode-p
		      #'meow-motion-mode-p
		      #'meow-keypad-mode-p))

(when (featurep 'nano-modeline)
  (setq rime-title " ㄓ")
  (defun +nano-modeline-rime-indicator (args)
    (cl-destructuring-bind (left right face-prefix) args
      (let* ((face (nano-modeline--base-face face-prefix))
             (left (append left '((rime-lighter)))))
        (dolist (rime-face '(rime-indicator-face
                             rime-indicator-dim-face))
          (eval `(face-spec-set ',rime-face '((t (:inherit ,face))))))
        (list left right face-prefix))))

  (advice-add #'nano-modeline--make :filter-args #'+nano-modeline-rime-indicator))
