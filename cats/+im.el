;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-im
  (:color teal :title (+with-icon "nf-md-chat" "Instant Messaging"))
  ("" ()))

(defvar cat-telega-contrib-load-path
  (expand-file-name "contrib" (file-name-directory (locate-library "telega"))))

(use-package telega
  :hook
  (telega-load . telega-mode-line-mode)
  :bind
  (:map telega-prefix-map
        ("k" . #'telega-kill))
  (:map telega-root-mode-map
        ("j" . #'telega-chat-with))
  (:map telega-msg-button-map
        ("SPC" . nil))
  :init
  (when IS-WSL
    (setq telega-docker-run-command "docker run --security-opt apparmor=unconfined -i -u %u -v %w:%w -v /tmp/.X11-unix:/tmp/.X11-unix -v $XAUTHORITY:$XAUTHORITY -v /var/run/dbus:/var/run/dbus -e DISPLAY=$DISPLAY -e XAUTHORITY=$XAUTHORITY --net=host %i"))
  :custom
  (telega-use-docker "podman")
  (telega-docker-volumes nil)
  (telega-docker-run-arguments "--platform linux/amd64 --userns=keep-id")
  (telega-use-images t)
  (telega-emoji-use-images nil)
  (telega-symbol-video-chat-active "🔊")
  (telega-symbol-video-chat-passive "🔈")
  (telega-video-player-command "mpv")
  (telega-chat-input-markups '("markdown2" nil))
  (telega-open-file-function #'org-open-file)
  (telega-completing-read-function completing-read-function))

(use-package telega-transient
  :load-path cat-telega-contrib-load-path
  :commands #'telega-transient-telega
  :pretty-hydra
  (cat-im
   ("Telegram"
    (("t" #'telega-transient-telega "telega"))))
  :config
  (telega-transient-mode))

(use-package telega-mnz
  :load-path cat-telega-contrib-load-path
  :hook (telega-load . global-telega-mnz-mode))

(use-package ement
  :init
  (defvar-keymap ement-prefix-map
    :doc "Keymap for ement."
    :name "Ement"
    :prefix 'ement-prefix
    "e" #'ement-connect
    "k" #'ement-kill-buffers
    "l" #'ement-list-rooms)
  :custom
  (ement-save-sessions t)
  :pretty-hydra
  (cat-im
   ("Matrix"
    (("e" #'ement-prefix "ement")))))

(use-package jabber
  :disabled)
