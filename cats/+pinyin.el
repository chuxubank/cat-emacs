;; -*- lexical-binding: t; -*-

(use-package pinyin-search
  :bind
  (:map search-map
        ("p" . #'pinyin-search)))
