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

(setq-default
 package-native-compile IS-CI
 package-archives (append
                   (assoc-default cat-package-mirror package-mirror-alist)
                   '(("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")))
 package-archive-priorities '(("gnu" . 5)
                              ("melpa" . 3)
                              ("jcs-elpa" . 0)))

(provide 'cat-package-archives)
