(use-package ns-auto-titlebar
  :config
  (and (or (daemonp)
           (display-graphic-p))
       (ns-auto-titlebar-mode +1)))

