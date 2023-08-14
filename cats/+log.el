;; -*- lexical-binding: t; -*-

(use-package logview
  :mode ("/log\\b" . logview-mode)
  :custom
  (datetime-timezone 'Asia/Shanghai)
  (logview-cache-filename (concat cat-cache-dir "logview-cache.extmap"))
  (logview-views-file (concat cat-etc-dir "logview.views")))
