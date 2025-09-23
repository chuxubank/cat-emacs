;; -*- lexical-binding: t; -*-

(require 'leetcode)
(require 'dom)
(require 'shr)

(defun leetcode--html-to-org (html)
  "Convert LeetCode HTML problem content to org format using pandoc."
  (let* ((infile  (make-temp-file "leetcode-html-" nil ".html"))
         (outfile (make-temp-file "leetcode-org-" nil ".org")))
    (with-temp-file infile
      (insert html))
    (with-temp-buffer
      (unless (zerop (call-process "pandoc" nil t nil
                                   "-f" "html" "-t" "org"
                                   "-o" outfile infile))
        (error "Pandoc failed: %s" (buffer-string))))
    (with-temp-buffer
      (insert-file-contents outfile)
      (buffer-string))))

(aio-defun roam-capture-leetcode (id)
  "Capture a LeetCode problem into org-roam by problem ID."
  (interactive "sEnter LeetCode problem ID: ")
  (let* ((problem (leetcode--get-problem-by-id id))
         (slug (leetcode-problem-title-slug problem))
         (problem-with-title (aio-await (leetcode--ensure-question-title problem)))
         (problem-with-content (aio-await (leetcode--ensure-question-content problem)))
         (problem-with-snippets (aio-await (leetcode--ensure-question-snippets problem)))
         (title (leetcode-problem-title problem-with-title))
         (difficulty (leetcode-problem-difficulty problem))
         (tags (string-join (leetcode-problem-tags problem) ":"))
         (content (leetcode--html-to-org (leetcode-problem-content problem-with-content)))
         (snippets (leetcode-problem-snippets problem-with-snippets))
         (snippet (progn
                    (leetcode--set-lang snippets)
                    (seq-find (lambda (s)
                                (equal (leetcode-snippet-lang-slug s) leetcode--lang))
                              snippets)))
         (template-code (leetcode-snippet-code snippet))
         (data `(
                 :number ,id
                 :title ,title
                 :slug ,slug
                 :difficulty ,difficulty
                 :tags ,tags
                 :content ,content
                 :template-code ,template-code
                 :lang ,leetcode--lang)))
    (org-roam-capture-
     :keys "l"
     :info data
     :node (org-roam-node-create :title title)
     :props data
     :templates org-roam-capture-ref-templates)))
