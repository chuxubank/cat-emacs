;; -*- lexical-binding: t; -*-

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
                                 ("glossary" . "${glossary:释义}")
                                 ("sentence" . "${sentence_bold:标粗的原文例句}"))))

(use-package osx-dictionary
  :when IS-MAC)

(defvar-keymap cat-dict-map
  :doc "Keymap for cat dict commands."
  :name "Dictionary"
  :prefix 'cat-dict-prefix
  "a" #'anki-vocabulary
  "b" #'bing-dict-brief
  "o" #'osx-dictionary-search-pointer
  "O" #'osx-dictionary-search-input)
