(use-package elfeed
  :commands #'elfeed
  :config
  (setq elfeed-search-filter "@2-week-ago -nsfw -buy "))

(use-package elfeed-org
  :after elfeed
  :config
  (setq rmh-elfeed-org-files (list (expand-file-name "elfeed.org" org-directory)))
  (elfeed-org))
