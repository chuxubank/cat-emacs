(use-package telega
  :commands #'telega)

(when IS-LINUX
  (setq telega-use-docker t))

(setq
 telega-symbol-folder "📁"
 telega-symbol-linked "🔗"
 telega-symbol-keyboard "⌨"
 telega-symbol-reply "↩"
 telega-symbol-forward "⏩"
 telega-chat-input-markups '("markdown2" nil)
 telega-filter-default '(and main (custom "Focus")))

(when (featurep 'selectrum)
  (setq telega-completing-read-function #'selectrum-completing-read))

(define-key global-map (kbd "C-c t") telega-prefix-map)
(define-key global-map (kbd "C-c t k") #'telega-kill)

(with-eval-after-load 'telega
  (when cat-alt-code-font
    (set-face-attribute 'telega-entity-type-pre nil :family cat-alt-code-font)
    (set-face-attribute 'telega-entity-type-code nil :family cat-alt-code-font)
    (set-face-attribute 'telega-webpage-fixed nil :family cat-alt-code-font))
  (add-to-list
   'telega-filters-custom
   '("Focus" . (not (folder "NSFW" "Proxy" "Coin")))))

(defun cat-telega-chat-mode ()
  (set (make-local-variable 'company-backends)
       (append (list telega-emoji-company-backend
                     'telega-company-username
                     'telega-company-hashtag)
               (when (telega-chat-bot-p telega-chatbuf--chat)
		 '(telega-company-botcmd))))
  (company-mode 1))

(add-hook 'telega-chat-mode-hook #'cat-telega-chat-mode)
