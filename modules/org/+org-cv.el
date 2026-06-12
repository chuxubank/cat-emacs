;; -*- lexical-binding: t; -*-

(use-package ox-altacv
  :vc (org-cv :url "https://gitlab.com/Titan-C/org-cv")
  :demand t
  :after org)

(use-package ox-moderncv
  :vc (org-cv :url "https://gitlab.com/Titan-C/org-cv")
  :demand t
  :after org)

(use-package ox-awesomecv
  :vc (org-cv :url "https://gitlab.com/Titan-C/org-cv")
  :demand t
  :after org)

(with-eval-after-load 'org-cv-utils
  (defun org-cv-utils-org-timestamp-to-shortdate (date_str)
    "Format org-mode timestamp DATE_STR into a locale-aware short form date.
English: Aug 2012
Chinese: 2012 年 8 月"
    (if (string-match (org-re-timestamp 'all) date_str)
        (let* ((dte   (org-parse-time-string date_str))
               (month (nth 4 dte))
               (year  (nth 5 dte))
               (lang  org-export-default-language))
          (if (string-prefix-p "zh" lang)
              (format "%d 年 %d 月" year month)
            (concat
             (calendar-month-name month 'abbreviate)
             " "
             (number-to-string year))))
      date_str)))
