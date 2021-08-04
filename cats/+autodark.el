(defun is-dark-mode()
  (when IS-WINDOWS
    (string-equal "0" (string-trim (shell-command-to-string "powershell -C Get-ItemPropertyValue -Path HKCU://Software/Microsoft/Windows/CurrentVersion/Themes/Personalize -Name AppsUseLightTheme")))))

(add-hook 'after-init-hook (lambda ()
			     (when (is-dark-mode)
			       (load-theme 'nord t))))
