;; -*- lexical-binding: t; -*-

(unless (or EMACS29+ (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'ignore-builtin)

(setq-default
 use-package-always-ensure (not IS-CI)
 use-package-always-defer t
 use-package-vc-prefer-newest t)

(use-package system-packages)
(use-package delight)
(use-package major-mode-hydra
  :demand t
  :init
  (defun cat-major-mode-hydra-title-generator (_)
    `(+with-mode-icon major-mode (s-concat (format-mode-line mode-name) " Commands")))
  :custom
  (major-mode-hydra-invisible-quit-key "q")
  (major-mode-hydra-title-generator #'cat-major-mode-hydra-title-generator))

(unless (or EMACS30+ (package-installed-p 'vc-use-package))
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

;; Ref @twlz0ne https://emacs-china.org/t/elpa/18226
(defun cat-find-fastest-elpa-mirror ()
  "Find the fatest elpa mirror."
  (interactive)
  (require 'request)
  (with-output-to-temp-buffer "*Elpa mirror test*"
    (princ "  score (s)  mirror                        last updated\n")
    (princ "-----------  ----------------------------  ------------------\n"))
  (dolist (mirror package-mirror-alist)
    (let ((url (cdr (assoc "melpa" mirror)))
          (begin-time (float-time (current-time)))
          (request-backend (car '(curl url-retrieve))))
      (request (concat url "archive-contents")
        :timeout 30
        :complete
        (cl-function
         (lambda (&key response symbol-status &allow-other-keys)
           (with-current-buffer "*Elpa mirror test*"
             (goto-char (point-max))
             (let ((inhibit-read-only t))
               (insert (format "%11s  %-29s [%s]\n"
                               (if (eq symbol-status 'success)
                                   (format
                                    "%6fs"
                                    (- (float-time (current-time)) begin-time))
                                 symbol-status)
                               (url-host (url-generic-parse-url url))
                               (if (eq symbol-status 'success)
                                   (request-response-header response "Last-Modified"))))))))))))
