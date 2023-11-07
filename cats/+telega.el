;; -*- lexical-binding: t; -*-

(use-package telega
  :defer t)

(when IS-WSL
  (setq telega-docker-run-command "docker run --security-opt apparmor=unconfined -i -u %u -v %w:%w -v /tmp/.X11-unix:/tmp/.X11-unix -v $XAUTHORITY:$XAUTHORITY -v /var/run/dbus:/var/run/dbus -e DISPLAY=$DISPLAY -e XAUTHORITY=$XAUTHORITY --net=host %i"))

(setq
 telega-use-docker t
 telega-use-images t
 telega-emoji-use-images nil
 telega-symbol-video-chat-active "🔊"
 telega-symbol-video-chat-passive "🔈"
 telega-video-player-command "mpv"
 telega-chat-input-markups '("markdown2" nil)
 telega-open-file-function #'org-open-file)

(defun +telega-chat-mode ()
  (set (make-local-variable 'company-backends)
       (append (list telega-emoji-company-backend
                     'telega-company-username
                     'telega-company-hashtag)
               (when (telega-chat-bot-p telega-chatbuf--chat)
                 '(telega-company-botcmd))))
  (company-mode 1))

(when (featurep 'company)
  (add-hook 'telega-chat-mode-hook #'+telega-chat-mode))

(with-eval-after-load 'telega
  (require 'telega-mnz)
  (global-telega-mnz-mode)
  (when cat-alt-code-font
    (set-face-attribute 'telega-entity-type-pre nil :family cat-alt-code-font)
    (set-face-attribute 'telega-entity-type-code nil :family cat-alt-code-font)
    (set-face-attribute 'telega-webpage-fixed nil :family cat-alt-code-font))
  (define-key telega-msg-button-map (kbd "SPC") nil))

(define-key telega-prefix-map "k" #'telega-kill)
(defalias 'telega-prefix telega-prefix-map)
