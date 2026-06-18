;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-im
  (:color teal :title (+with-icon "nf-md-chat" "Instant Messaging"))
  ("" ()))

(use-package telega
  :hook
  (telega-load . telega-mode-line-mode)
  :commands #'telega-transient--prefix-telega-prefix-map
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
  (+add-to-list-multi 'cat-custom-reevaluate-setting-list
                      'telega-chat-show-avatars
                      'telega-user-show-avatars)
  :custom
  (telega-chat-input-markups '("markdown2" nil))
  (telega-docker-run-arguments (concat "--platform linux/amd64" (when (eq "podman" telega-use-docker) " --userns=keep-id")))
  (telega-docker-volumes nil)
  (telega-emoji-use-images nil)
  (telega-msg-heading-trail 'date-and-status)
  (telega-open-file-function #'org-open-file)
  (telega-symbol-video-chat-active "")
  (telega-symbol-video-chat-passive "")
  (telega-use-docker (if (eq HOST_ENV 'aa) "podman" "docker"))
  (telega-use-images t)
  (telega-video-player-command "mpv")
  :pretty-hydra
  (cat-im
   ("Telegram"
    (("t" #'telega-transient--prefix-telega-prefix-map "telega"))))
  :config
  (telega-transient-keymaps-mode))

(defvar cat-telega-contrib-load-path
  (when (package-installed-p 'telega)
    (expand-file-name "contrib" (file-name-directory (locate-library "telega")))))

(use-package telega-mnz
  :load-path cat-telega-contrib-load-path
  :hook (telega-load . global-telega-mnz-mode))

(use-package ement
  :custom
  (ement-save-sessions t)
  :pretty-hydra
  ((:color teal :title "Ement")
   ("Action"
    (("e" #'ement-connect "connect")
     ("k" #'ement-kill-buffers "kill")
     ("l" #'ement-list-rooms "rooms"))))
  (cat-im
   ("Matrix"
    (("e" #'ement-hydra/body "ement")))))

(use-package jabber
  :disabled)
