;; -*- lexical-binding: t; -*-

(use-package csv-mode
  :font-role table)

(use-package rainbow-csv
  :pin jcs-elpa
  :hook ((csv-mode tsv-mode) . rainbow-csv-mode))
