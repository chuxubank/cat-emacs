;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-eudc
  (:color teal :title (+with-icon "nf-md-contacts" "EUDC"))
  ("" ()))

(use-package eudc
  :ensure nil
  :init
  (with-eval-after-load 'message
    (define-key message-mode-map [(control ?c) (tab)] 'eudc-expand-try-all))
  (with-eval-after-load 'sendmail
    (define-key mail-mode-map [(control ?c) (tab)] 'eudc-expand-try-all))
  :pretty-hydra
  (cat-eudc
   ("Action"
    (("t" #'eudc-expand-try-all "expand")
     ("f" #'eudc-query-form "query form")
     ("p" #'eudc-get-phone "get phone")
     ("e" #'eudc-get-email "get email")
     ("l" #'eudc-get-attribute-list "get list")))))

(use-package eudcb-macos-contacts
  :when IS-MAC
  :ensure nil
  :config
  (add-to-list 'eudc-server-hotlist '("localhost" . macos-contacts)))

(use-package bbdb
  :custom
  (bbdb-file (expand-file-name "bbdb.gpg" cat-org-directory))
  :pretty-hydra
  (cat-eudc
   ("BBDB"
    (("b" #'bbdb "bbdb")
     ("a" #'bbdb-create "create"))))
  :config
  (bbdb-initialize)
  (add-to-list 'eudc-server-hotlist '("" . bbdb)))
