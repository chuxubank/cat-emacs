;; -*- lexical-binding: t; -*-

(mode-transient-define-prefix cat-eudc ()
  :description (+with-icon "nf-md-contacts" nil " EUDC"))

(use-package eudc
  :ensure nil
  :init
  (with-eval-after-load 'message
    (define-key message-mode-map [(control ?c) (tab)] 'eudc-expand-try-all))
  (with-eval-after-load 'sendmail
    (define-key mail-mode-map [(control ?c) (tab)] 'eudc-expand-try-all))
  :custom
  (eudc-strict-return-matches nil)
  :transient
  (cat-eudc
   ["Action"
    ("t" "expand" eudc-expand-try-all)
    ("f" "query form" eudc-query-form)
    ("p" "get phone" eudc-get-phone)
    ("e" "get email" eudc-get-email)
    ("l" "get list" eudc-get-attribute-list)]))

(use-package eudcb-macos-contacts
  :when IS-MAC
  :ensure nil
  :config
  (add-to-list 'eudc-server-hotlist '("localhost" . macos-contacts)))

(use-package bbdb
  :custom
  (bbdb-file (expand-file-name "bbdb.gpg" cat-org-directory))
  :transient
  (cat-eudc
   ["BBDB"
    ("b" "bbdb" bbdb)
    ("a" "create" bbdb-create)])
  :config
  (bbdb-initialize)
  (with-eval-after-load 'eudc
    (add-to-list 'eudc-server-hotlist '("" . bbdb))))
