;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-language
  (:color teal :title (+with-icon "nf-fa-language" "Language"))
  ("" ()))

(use-package bing-dict
  :config
  (setq bing-dict-vocabulary-save t
        bing-dict-show-thesaurus 'both
        bing-dict-vocabulary-file (expand-file-name "vocabulary.org" cat-org-directory))
  :pretty-hydra
  (cat-language
   ("Dictionary"
    (("b" #'bing-dict-brief "bing")))))

(use-package anki-vocabulary
  :custom
  (anki-vocabulary-deck-name "Inbox")
  (anki-vocabulary-model-name "AutoCopy")
  (anki-vocabulary-field-alist '(("expression" . "${expression:单词}")
                                 ("glossary"   . "${glossary:释义}")
                                 ("sentence"   . "${sentence_bold:标粗的原文例句}")))
  :pretty-hydra
  (cat-language
   ("Dictionary"
    (("a" #'anki-vocabulary "anki")))))

(use-package osx-dictionary
  :when IS-MAC
  :pretty-hydra
  (cat-language
   ("Dictionary"
    (("o" #'osx-dictionary-search-pointer "osx")
     ("O" #'osx-dictionary-search-input "osx input")))))

(use-package go-translate
  :custom
  (gt-langs '(en zh))
  (gt-preset-translators
   `((auto
      . ,(gt-translator
          :taker   (list (gt-taker :pick nil :if 'selection)
                         (gt-taker :text 'paragraph :if '(Info-mode help-mode))
                         (gt-taker :text 'buffer :pick 'fresh-word :if 'read-only)
                         (gt-taker :text 'word))
          :engines (list (gt-stardict-engine :dir (getenv "STARDICT_DATA_DIR") :if 'word)
                         (gt-bing-engine :if 'word)
                         (gt-google-engine :if 'no-word))
          :render  (list (gt-posframe-pop-render :if 'selection)
                         (gt-overlay-render :if 'read-only)
                         (gt-insert-render :if (lambda (translator) (member (buffer-name) '("COMMIT_EDITMSG"))))
                         (gt-alert-render :if '(and org-mode (or not-selection (and read-only parts))))
                         (gt-buffer-render))))
     (cjk
      . ,(gt-translator
          :taker (gt-taker :langs '(jp kr zh)
                           :text 'sentence)
          :engines (gt-google-engine)
          :render (gt-insert-render)))
     (hard-words
      . ,(gt-translator
          :taker (gt-taker :langs '(en zh)
                           :text 'buffer
                           :pick 'word
                           :pick-pred (lambda (w) (length> w 6)))
          :engines (gt-google-engine)
          :render (gt-overlay-render :type 'help-echo)))))
  :pretty-hydra
  (cat-language
   ("Translate"
    (("gg" #'gt-do-translate "go translate")
     ("gs" #'gt-do-setup "go translate setup")
     ("gS" #'gt-do-speak "go translate speak")
     ("gd" #'gt-delete-render-overlays "go translate delete ov")))))

(use-package immersive-translate
  :disabled)
