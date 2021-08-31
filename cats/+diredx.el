(add-hook 'dired-load-hook
          (lambda ()
	    ;; Bind dired-x-find-file.
            (setq dired-x-hands-off-my-keys nil
		  dired-guess-shell-gnutar "gtar")
            (load "dired-x")
            ))

(add-hook 'dired-mode-hook
          (lambda ()
	    (when IS-WINDOWS
	      (setq dired-omit-files
		    (concat dired-omit-files "\\|^ntuser\\(\\.dat\\|\\.ini\\).*")))
            (dired-omit-mode 1)
            ))
