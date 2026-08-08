;; -*- lexical-binding: t; -*-

(mode-transient-define-prefix cat-language ()
  :description (+with-icon "nf-fa-language" nil " Language"))

(use-package bing-dict
  :config
  (setq bing-dict-vocabulary-save t
        bing-dict-show-thesaurus 'both
        bing-dict-vocabulary-file (expand-file-name "vocabulary.org" cat-org-directory))
  :transient
  (cat-language
   ["Dictionary"
    ("b" "bing" bing-dict-brief)]))

(use-package anki-vocabulary
  :custom
  (anki-vocabulary-deck-name "Inbox")
  (anki-vocabulary-model-name "AutoCopy")
  (anki-vocabulary-field-alist '(("expression" . "${expression:单词}")
                                 ("glossary"   . "${glossary:释义}")
                                 ("sentence"   . "${sentence_bold:标粗的原文例句}")))
  :transient
  (cat-language
   ["Dictionary"
    ("a" "anki" anki-vocabulary)]))

(use-package osx-dictionary
  :when IS-MAC
  :transient
  (cat-language
   ["Dictionary"
    ("o" "osx" osx-dictionary-search-pointer)
    ("O" "osx input" osx-dictionary-search-input)]))

(use-package gt
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
  :transient
  (cat-language
   ["Translate"
    ("gg" "go translate" gt-translate)
    ("gs" "go translate setup" gt-setup)
    ("gS" "go translate speak" gt-speak)
    ("gd" "go translate delete ov" gt-delete-render-overlays)]))

(use-package immersive-translate
  :cat it)
