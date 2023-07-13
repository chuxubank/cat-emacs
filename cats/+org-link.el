;; -*- lexical-binding: t; -*-

(use-package org-yt
  :straight (org-yt :host github :repo "TobiasZawada/org-yt")
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
  :straight (org-dial :host github :repo "mistrey/org-dial")
  :after org
  :config
  (when IS-MAC
    (setq org-dial-program "open tel:")))

(use-package org-cliplink
  :bind
  (:map org-mode-map
        ("C-c n l" . org-cliplink)))