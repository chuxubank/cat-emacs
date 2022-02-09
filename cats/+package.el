;; -*- lexical-binding: t; -*-

(require 'package)

(defconst package-mirror-alist
  '((default
     ("gnu"		. "https://elpa.gnu.org/packages/")
     ("nongnu"		. "https://elpa.nongnu.org/nongnu/")
     ("melpa"		. "https://melpa.org/packages/")
     ("melpa-stable"	. "https://stable.melpa.org/packages/"))
    (emacs-china
     ("gnu"		. "http://elpa.zilongshanren.com/gnu/")
     ("nongnu"		. "http://elpa.zilongshanren.com/nongnu/")
     ("melpa"		. "http://elpa.zilongshanren.com/melpa/")
     ("melpa-stable"	. "http://elpa.zilongshanren.com/stable-melpa/"))
    (tsinghua
     ("gnu"		. "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
     ("nongnu"		. "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
     ("melpa"		. "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
     ("melpa-stable"	. "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/"))
    (ustc
     ("gnu"		. "https://mirrors.ustc.edu.cn/elpa/gnu/")
     ("nongnu"		. "https://mirrors.ustc.edu.cn/elpa/nongnu/")
     ("melpa"		. "https://mirrors.ustc.edu.cn/elpa/melpa/")
     ("melpa-stable"	. "https://mirrors.ustc.edu.cn/elpa/melpa-stable/"))
    (sjtu
     ("gnu"		. "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/")
     ("nongnu"		. "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu/")
     ("melpa"		. "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/")
     ("melpa-stable"	. "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa-stable/")
     ("marmalade"	. "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/marmalade/")
     ("sunrise"		. "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/sunrise-commander/")
     ("user42"		. "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/user42/"))))

(setq package-check-signature nil
      package-archives (assoc-default 'sjtu package-mirror-alist))

(require 'use-package)
(setq use-package-always-ensure t)

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
        :timeout 10
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
