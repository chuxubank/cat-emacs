;;; leetcode-org-roam.el --- Capture LeetCode problems into Org-roam -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Misaka

;; Author: Misaka <chuxubank@qq.com>
;; Maintainer: Misaka <chuxubank@qq.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (leetcode "0.2") (org-roam "2.0") (aio "1.0"))
;; Keywords: tools, outlines, leetcode, org-roam
;; Homepage: https://github.com/chuxubank/leetcode-org-roam.el

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package integrates `leetcode.el' with `org-roam', making it
;; possible to capture LeetCode problems directly into your Org-roam
;; knowledge base.
;;
;; Features:
;;
;; - Fetch LeetCode problem metadata (title, slug, difficulty, tags, etc.)
;; - Convert problem content (HTML) to Org format using Pandoc
;; - Capture problem into Org-roam with :PROPERTIES:
;; - Insert code template snippet based on `leetcode-prefer-language'
;;
;; Usage:
;;
;;   (require 'leetcode-org-roam)
;;
;;   ;; Then run interactively:
;;   M-x leetcode-org-roam-capture
;;
;; You will be prompted for the LeetCode problem ID, and the
;; corresponding problem will be inserted into Org-roam using your
;; `org-roam-capture-ref-templates'.

;;; Code:

(require 'leetcode)
(require 'org-roam-capture)
(require 'aio)

(defgroup leetcode-org-roam nil
  "Capture LeetCode problems into Org-roam."
  :prefix "leetcode-org-roam-"
  :group 'tools
  :link '(url-link :tag "GitHub" "https://github.com/chuxubank/leetcode-org-roam"))

(defcustom leetcode-org-roam-pandoc-command "pandoc"
  "Pandoc command used to convert HTML problem content to Org."
  :type 'string
  :group 'leetcode-org-roam)

(defcustom leetcode-org-roam-capture-key "l"
  "The key to use when do Org-roam capture."
  :type 'string
  :group 'leetcode-org-roam)

(defun leetcode-org-roam--html-to-org (html)
  "Convert LeetCode HTML problem content to Org format using Pandoc.
Requires `pandoc` to be installed and available in PATH."
  (let* ((infile  (make-temp-file "leetcode-html-" nil ".html"))
         (outfile (make-temp-file "leetcode-org-" nil ".org")))
    (with-temp-file infile
      (insert html))
    (with-temp-buffer
      (unless (zerop (call-process leetcode-org-roam-pandoc-command nil t nil
                                   "-f" "html" "-t" "org"
                                   "-o" outfile infile))
        (error "Pandoc failed: %s" (buffer-string))))
    (with-temp-buffer
      (insert-file-contents outfile)
      (buffer-string))))

;;;###autoload
(aio-defun leetcode-org-roam-capture (id)
  "Capture a LeetCode problem into Org-roam by problem ID.

Fetches metadata, problem statement, and starter code using `leetcode'.
Converts content to Org format and creates or updates an Org-roam node.

If a node with the same :ref (problem URL) exists, the capture will open
that node instead of creating a new one.

Properties stored in the node:
  :number, :ref, :slug, :title, :title-slug, :difficulty, :tags,
  :content, :template-code, :lang"
  (interactive (list (read-string "Org roam capture leetcode problem by problem id: "
                                  (when (derived-mode-p 'leetcode--problems-mode)
                                    (leetcode--get-current-problem-id)))))
  (unless (get-buffer leetcode--buffer-name)
    (aio-await (leetcode--ensure-login))
    (aio-await (leetcode-refresh-fetch)))
  (let* ((problem (leetcode--get-problem-by-id id))
         (slug (leetcode-problem-title-slug problem))
         (problem-with-title (aio-await (leetcode--ensure-question-title problem)))
         (problem-with-content (aio-await (leetcode--ensure-question-content problem)))
         (problem-with-snippets (aio-await (leetcode--ensure-question-snippets problem)))
         (title (leetcode-problem-title problem-with-title))
         (link (leetcode--problem-link title))
         (title-slug (leetcode--slugify-title title))
         (difficulty (leetcode-problem-difficulty problem))
         (tags (leetcode-problem-tags problem))
         (content (leetcode-org-roam--html-to-org (leetcode-problem-content problem-with-content)))
         (snippets (leetcode-problem-snippets problem-with-snippets))
         (snippet (progn
                    (leetcode--set-lang snippets)
                    (seq-find (lambda (s)
                                (equal (leetcode-snippet-lang-slug s) leetcode--lang))
                              snippets)))
         (template-code (leetcode-snippet-code snippet))
         (data `(
                 :number ,id
                 :ref ,link
                 :slug ,slug
                 :title ,title
                 :title-slug ,title-slug
                 :difficulty ,difficulty
                 :tags ,tags
                 :content ,content
                 :template-code ,template-code
                 :lang ,leetcode--lang))
         (node (org-roam-node-from-ref link))
         (goto (when node '(4))))
    (org-roam-capture-
     :goto goto
     :info data
     :keys leetcode-org-roam-capture-key
     :templates org-roam-capture-ref-templates
     :node (or node (org-roam-node-create :title title))
     :props data)))

(provide 'leetcode-org-roam)

;;; leetcode-org-roam.el ends here
