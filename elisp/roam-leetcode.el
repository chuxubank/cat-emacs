;; -*- lexical-binding: t; -*-

(require 'leetcode)
(require 'dom)
(require 'shr)

(defun leetcode--html-to-org (html)
  "Convert LeetCode HTML problem content to org format."
  (with-temp-buffer
    (insert html)
    (goto-char (point-min))
    (while (re-search-forward "<pre" nil t)
      (replace-match "<pre class=\"src\">"))
    (goto-char (point-min))
    (shr-render-region (point-min) (point-max))
    (buffer-string)))

(defun leetcode-roam-capture (id)
  "Capture a LeetCode problem into org-roam by problem ID."
  (interactive "sEnter LeetCode problem ID: ")
  (let* ((problem (leetcode--get-problem-by-id id))
         (title (leetcode-problem-title problem))
         (slug (leetcode-problem-title-slug problem))
         (difficulty (leetcode-problem-difficulty problem))
         (tags (string-join (leetcode-problem-tags problem) ", "))
         (content (leetcode--html-to-org (leetcode-problem-content problem)))
         (snippets (leetcode-problem-snippets problem))
         (snippet (progn
                    (leetcode--set-lang snippets)
                    (seq-find (lambda (s)
                                (equal (leetcode-snippet-lang-slug s) leetcode--lang))
                              snippets)))
         (data `((id . ,id)
                 (title . ,title)
                 (slug . ,slug)
                 (difficulty . ,difficulty)
                 (tags . ,tags)
                 (content . ,content)
                 (snippet . ,snippet)
                 (lang . ,leetcode--lang))))
    (org-roam-capture-
     :keys "l"
     :info data
     :node (org-roam-node-create :title title)
     :props data
     :templates org-roam-capture-ref-templates)))
