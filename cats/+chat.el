;; -*- lexical-binding: t; -*-

(use-package telega
  :bind
  (:map telega-prefix-map
        ("k" . #'telega-kill))
  (:map telega-root-mode-map
        ("j" . #'telega-chat-with))
  (:map telega-msg-button-map
        ("SPC" . nil))
  :init
  (defalias 'telega-prefix telega-prefix-map)
  :custom
  (telega-use-docker t)
  (telega-use-images t)
  (telega-emoji-use-images nil)
  (telega-symbol-video-chat-active "🔊")
  (telega-symbol-video-chat-passive "🔈")
  (telega-video-player-command "mpv")
  (telega-chat-input-markups '("markdown2" nil))
  (telega-open-file-function #'org-open-file)
  (telega-completing-read-function completing-read-function)
  (when IS-WSL
    (setq telega-docker-run-command "docker run --security-opt apparmor=unconfined -i -u %u -v %w:%w -v /tmp/.X11-unix:/tmp/.X11-unix -v $XAUTHORITY:$XAUTHORITY -v /var/run/dbus:/var/run/dbus -e DISPLAY=$DISPLAY -e XAUTHORITY=$XAUTHORITY --net=host %i"))
  :config
  (require 'telega-mnz)
  (global-telega-mnz-mode))
