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
  :major-transient
  (org-mode
   ["Toggle"
    ("tl" "link display" org-toggle-link-display :transient t)
    ("tL" "link preview" org-link-preview :transient t)]))

(use-package org-yt
  :vc (:url "https://github.com/TobiasZawada/org-yt")
  :demand t
  :after org
  :custom
  (org-yt-cache-directory (concat cat-cache-dir "yt-cache")))

(use-package org-remoteimg
  :vc (:url "https://github.com/gaoDean/org-remoteimg")
  :demand t
  :after org
  :config
  (advice-add #'org-link-preview-region :after #'org-display-user-inline-images))

(use-package org-imgtog
  :vc (:url "https://github.com/gaoDean/org-imgtog")
  :hook (org-mode . org-imgtog-mode))

(defun cat/org-inline-image-data-fn (_protocol link _description)
  "Interpret LINK as base64-encoded image data."
  (base64-decode-string link))

(with-eval-after-load 'org
  (org-link-set-parameters "img" :image-data-fun #'cat/org-inline-image-data-fn))

(use-package org-dial
  :vc (:url "https://github.com/mistrey/org-dial")
  :demand t
  :after org
  :config
  (when IS-MAC
    (setq org-dial-program "open tel:")))

(use-package org-cliplink
  :major-transient
  (org-mode
   ["Plugin"
    ("c" "cliplink" org-cliplink)]))

(use-package org-link-beautify
  :pin melpa-stable
  :demand t
  :after org
  :custom
  (org-link-beautify-thumbnails-dir 'user-home)
  :major-transient
  (org-mode
   ["Mode"
    ("b" "link beautify" org-link-beautify-mode :transient t)]))
