;; -*- lexical-binding: t; -*-

(use-package org-yt
  :vc (org-yt
       :url "https://github.com/TobiasZawada/org-yt"
       :rev :newest)
  :demand t
  :custom
  (org-yt-cache-directory (concat cat-cache-dir "yt-cache"))
  :after org)

(defun +org-inline-image-data-fn (_protocol link _description)
  "Interpret LINK as base64-encoded image data."
  (base64-decode-string link))

(defun +org-http-image-data-fn (protocol link _description)
  "Interpret LINK as an URL to an image file."
  (when (and (image-type-from-file-name link)
             (not (eq org-display-remote-inline-images 'skip)))
    (if-let (buf (url-retrieve-synchronously (concat protocol ":" link)))
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "\r?\n\r?\n" nil t)
          (buffer-substring-no-properties (point) (point-max)))
      (message "Download of image \"%s\" failed" link)
      nil)))

(with-eval-after-load 'org
  (org-link-set-parameters "http"  :image-data-fun #'+org-http-image-data-fn)
  (org-link-set-parameters "https" :image-data-fun #'+org-http-image-data-fn)
  (org-link-set-parameters "img"   :image-data-fun #'+org-inline-image-data-fn))

(use-package org-dial
  :vc (org-dial
       :url "https://github.com/mistrey/org-dial"
       :rev :newest)
  :demand t
  :after org
  :config
  (when IS-MAC
    (setq org-dial-program "open tel:")))

(use-package org-cliplink)

;; Remember to install latest org via https://orgmode.org/org.html#Installation
(use-package org-link-beautify
  :hook
  (org-mode . org-link-beautify-mode)
  :custom
  (org-link-beautify-thumbnails-dir 'user-home)
  (org-link-beautify-display-overlay-info t)
  (org-link-beautify-image-preview t)
  :config
  (advice-add 'org-agenda-finalize :before #'org-link-beautify-disable)
  (advice-add 'org-agenda-finalize :after #'org-link-beautify-enable))
