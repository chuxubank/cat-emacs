;; -*- lexical-binding: t; -*-

(use-package ol
  :demand t
  :after org
  :ensure nil
  :bind
  (:map org-mode-map
        ("M-n" . org-next-link)
        ("M-p" . org-previous-link))
  :custom
  (org-link-abbrev-alist '(("wiki-zh" . "https://zh.wikipedia.org/wiki/%h")
                           ("wiki-en" . "https://en.wikipedia.org/wiki/%s")
                           ("github" . "https://github.com/%s")
                           ("google" . "https://goo.gle/%s")
                           ("bitbucket" . "https://bitbucket.org/%s")
                           ("bili". "https://bilibili.com/video/%s")
                           ("coursera". "https://www.coursera.org/%s")))
  :mode-hydra
  (org-mode
   ("Toggle"
    (("tl" org-toggle-link-display "link display" :color red)
     ("tL" org-link-preview "link preview" :color red)))))

(use-package org-yt
  :vc (:url "https://github.com/TobiasZawada/org-yt")
  :demand t
  :after org
  :custom
  (org-yt-cache-directory (concat cat-cache-dir "yt-cache")))

(use-package org-remoteimg
  :vc (org-remoteimg :url "https://github.com/gaoDean/org-remoteimg")
  :demand t
  :after org
  :config
  (advice-add #'org-link-preview-region :after #'org-display-user-inline-images))

(use-package org-imgtog
  :vc (org-imgtog :url "https://github.com/gaoDean/org-imgtog")
  :hook (org-mode . org-imgtog-mode))

(defun +org-inline-image-data-fn (_protocol link _description)
  "Interpret LINK as base64-encoded image data."
  (base64-decode-string link))

(with-eval-after-load 'org
  (org-link-set-parameters "img" :image-data-fun #'+org-inline-image-data-fn))

(use-package org-dial
  :vc (:url "https://github.com/mistrey/org-dial")
  :demand t
  :after org
  :config
  (when IS-MAC
    (setq org-dial-program "open tel:")))

(use-package org-cliplink
  :mode-hydra
  (org-mode
   ("Plugin"
    (("c" org-cliplink "cliplink")))))

(use-package org-link-beautify
  :demand t
  :after org
  :custom
  (org-link-beautify-thumbnails-dir 'user-home)
  :mode-hydra
  (org-mode
   ("Mode"
    (("b" org-link-beautify-mode "link beautify" :color red)))))
