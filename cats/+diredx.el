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

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)
