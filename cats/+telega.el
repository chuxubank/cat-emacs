(use-package telega
  :commands #'telega)

(setq telega-symbol-folder "📁"
      telega-symbol-reply "↫")

(when IS-WINDOWS
  (setq telega-chat-show-avatars nil
	telega-company-username-show-avatars nil
	telega-root-show-avatars nil
	telega-user-show-avatars nil))

(when (featurep 'selectrum)
  (setq telega-completing-read-function #'selectrum-completing-read))


(define-key global-map (kbd "C-c a t") telega-prefix-map)
