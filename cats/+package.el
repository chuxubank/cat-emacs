;; -*- lexical-binding: t; -*-

(require 'package)

(defcustom cat-package-mirror 'default
  "The package mirror to use for package.el."
  :group 'cat-emacs
  :type '(choice
          (const default)
          (const tsinghua)
          (const ustc)
          (const sjtu)))

(defconst package-mirror-alist
  '((default
     ("gnu". "https://elpa.gnu.org/packages/")
     ("gnu-devel". "https://elpa.gnu.org/devel/")
     ("nongnu". "https://elpa.nongnu.org/nongnu/")
     ("melpa". "https://melpa.org/packages/")
     ("melpa-stable". "https://stable.melpa.org/packages/"))
    (tsinghua ; https://mirrors.tuna.tsinghua.edu.cn/help/elpa/
     ("gnu". "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
     ("nongnu". "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
     ("melpa". "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
     ("melpa-stable". "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/"))
    (ustc ; https://mirrors.ustc.edu.cn/help/elpa.html
     ("gnu". "https://mirrors.ustc.edu.cn/elpa/gnu/")
     ("nongnu". "https://mirrors.ustc.edu.cn/elpa/nongnu/")
     ("melpa". "https://mirrors.ustc.edu.cn/elpa/melpa/"))
    (sjtu ; https://mirrors.sjtug.sjtu.edu.cn/docs/emacs-elpa
     ("gnu". "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/")
     ("nongnu". "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu/")
     ("melpa". "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/"))
    ))

(custom-set-variables
 '(package-check-signature nil)
 '(package-install-upgrade-built-in nil)
 '(package-native-compile IS-CI)
 '(package-archives (append
                     (assoc-default cat-package-mirror package-mirror-alist)
                     '(("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/"))))
 '(package-archive-priorities '(("gnu" . 5)
                                ("melpa" . 3)
                                ("jcs-elpa" . 0))))

(unless (or EMACS29+ (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'ignore-builtin)

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

(defun cat/package-vc-skip-if-same-hash (orig-fn pkg-desc)
  "Skip `package-vc-upgrade' if PKG-DESC is already at the latest commit.
Only applies to Git VC packages; otherwise, run ORIG-FN.

This version safely handles branches without upstream and can optionally fetch."
  (let* ((pkg-dir (package-desc-dir pkg-desc))
         (backend (vc-responsible-backend pkg-dir)))
    (if (not (eq backend 'Git))
        (funcall orig-fn pkg-desc)
      (let ((default-directory pkg-dir))
        (condition-case err
            (let* ((upstream (string-trim
                              (with-output-to-string
                                (vc-git-command standard-output nil nil
                                                "rev-parse" "--abbrev-ref" "@{u}")))))
              (if (string-empty-p upstream)
                  (funcall orig-fn pkg-desc)
                (vc-git-command t 0 nil "fetch" "--quiet")
                (let* ((local (string-trim
                               (with-output-to-string
                                 (vc-git-command standard-output nil nil "rev-parse" "HEAD"))))
                       (remote (string-trim
                                (with-output-to-string
                                  (vc-git-command standard-output nil nil "rev-parse" "@{u}")))))
                  (if (string= local remote)
                      (message "Package %s already up-to-date" (package-desc-name pkg-desc))
                    (funcall orig-fn pkg-desc)))))
          (error
           (funcall orig-fn pkg-desc)))))))


(advice-add 'package-vc-upgrade :around #'cat/package-vc-skip-if-same-hash)
