;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-language
  (:color teal :title (+with-icon "nf-fa-language" "Language"))
  ("Dictionary"
   (("a" #'anki-vocabulary "anki")
    ("b" #'bing-dict-brief "bing"))))

(use-package bing-dict
  :config
  (setq bing-dict-vocabulary-save t
        bing-dict-show-thesaurus 'both
        bing-dict-vocabulary-file (expand-file-name "vocabulary.org" cat-org-directory)))

(use-package anki-vocabulary
  :custom
  (anki-vocabulary-deck-name "Inbox")
  (anki-vocabulary-model-name "AutoCopy")
  (anki-vocabulary-field-alist '(("expression" . "${expression:单词}")
                                 ("glossary"   . "${glossary:释义}")
                                 ("sentence"   . "${sentence_bold:标粗的原文例句}"))))

(use-package osx-dictionary
  :when IS-MAC
  :pretty-hydra
  (cat-language
   ("Dictionary"
    (("o" #'osx-dictionary-search-pointer "osx")
     ("O" #'osx-dictionary-search-input "osx input")))))
