;; -*- lexical-binding: t; -*-

(require 'package)

(defcustom package-mirror 'default
  "The package mirror to use for package.el.")

(defconst package-mirror-alist
  '((default
     ("gnu". "https://elpa.gnu.org/packages/")
     ("gnu-devel". "https://elpa.gnu.org/devel/")
     ("nongnu". "https://elpa.nongnu.org/nongnu/")
     ("melpa". "https://melpa.org/packages/")
     ("melpa-stable". "https://stable.melpa.org/packages/"))
    (emacs-china ; https://elpamirror.emacs-china.org
     ("gnu". "http://1.15.88.122/gnu/")
     ("nongnu". "http://1.15.88.122/nongnu/")
     ("melpa". "http://1.15.88.122/melpa/")
     ("melpa-stable". "http://1.15.88.122/stable-melpa/"))
    (tsinghua ; https://mirrors.tuna.tsinghua.edu.cn/help/elpa/
     ("gnu". "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
     ("nongnu". "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
     ("melpa". "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
     ("melpa-stable". "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/"))
    (ustc ; https://mirrors.ustc.edu.cn/help/elpa.html
     ("gnu". "https://mirrors.ustc.edu.cn/elpa/gnu/")
     ("nongnu". "https://mirrors.ustc.edu.cn/elpa/nongnu/")
     ("melpa". "https://mirrors.ustc.edu.cn/elpa/melpa/")
     ("melpa-stable". "https://mirrors.ustc.edu.cn/elpa/melpa-stable/"))
    (sjtu ; https://mirrors.sjtug.sjtu.edu.cn/docs/emacs-elpa
     ("gnu". "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/")
     ("nongnu". "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu/")
     ("melpa". "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/")
     ("melpa-stable". "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa-stable/")
     ("marmalade". "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/marmalade/")
     ("sunrise". "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/sunrise-commander/")
     ("user42". "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/user42/"))
    ))

(setq package-check-signature nil
      package-install-upgrade-built-in t
      package-archives (assoc-default package-mirror package-mirror-alist))

(unless (or EMACS29+ (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(custom-set-variables
 '(use-package-always-ensure t)
 '(use-package-always-defer t))

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

(use-package project
  :ensure nil
  :commands #'project--find-in-directory
  :custom
  (project-list-file (concat cat-cache-dir "projects")))

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
