;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'package)
(require 'url-parse)

(defvar request-backend)

(defcustom cat-package-mirror 'default
  "The package mirror to use for package.el."
  :group 'cat-emacs
  :type '(choice
          (const default)
          (const tsinghua)
          (const ustc)
          (const sjtu)))

(defconst cat-package--mirrors
  '((default
     ("gnu" . "https://elpa.gnu.org/packages/")
     ("gnu-devel" . "https://elpa.gnu.org/devel/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/"))
    (tsinghua ; https://mirrors.tuna.tsinghua.edu.cn/help/elpa/
     ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
     ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
     ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
     ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/"))
    (ustc ; https://mirrors.ustc.edu.cn/help/elpa.html
     ("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
     ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
     ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/"))
    (sjtu ; https://mirrors.sjtug.sjtu.edu.cn/docs/emacs-elpa
     ("gnu" . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/")
     ("nongnu" . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu/")
     ("melpa" . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/")))
  "Package archives available for each `cat-package-mirror' value.")

(setq-default
 package-native-compile IS-CI
 package-archives (append
                   (alist-get cat-package-mirror cat-package--mirrors)
                   '(("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")))
 package-archive-priorities '(("gnu" . 5)
                              ("melpa" . 3)
                              ("jcs-elpa" . 0)))

(defun cat/find-fastest-elpa-mirror ()
  "Measure response times for the configured ELPA mirrors."
  (interactive)
  (require 'request)
  (with-output-to-temp-buffer "*Elpa mirror test*"
    (princ "  score (s)  mirror                        last updated\n")
    (princ "-----------  ----------------------------  ------------------\n"))
  (dolist (mirror cat-package--mirrors)
    (let ((url (alist-get "melpa" (cdr mirror) nil nil #'string=))
          (started-at (float-time))
          (request-backend 'curl))
      (request (concat url "archive-contents")
        :timeout 30
        :complete
        (cl-function
         (lambda (&key response symbol-status &allow-other-keys)
           (with-current-buffer "*Elpa mirror test*"
             (goto-char (point-max))
             (let ((inhibit-read-only t))
               (insert
                (format "%11s  %-29s [%s]\n"
                        (if (eq symbol-status 'success)
                            (format "%6fs" (- (float-time) started-at))
                          symbol-status)
                        (url-host (url-generic-parse-url url))
                        (when (eq symbol-status 'success)
                          (request-response-header
                           response "Last-Modified"))))))))))))

(provide 'cat-package-archives)
