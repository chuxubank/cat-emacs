(defun is-dark-mode()
  (when IS-WINDOWS
    (string-equal "0" (string-trim (shell-command-to-string "powershell -C Get-ItemPropertyValue -Path HKCU://Software/Microsoft/Windows/CurrentVersion/Themes/Personalize -Name AppsUseLightTheme"))))
  (when IS-LINUX
    (string-match-p "-dark" (shell-command-to-string "gsettings get org.gnome.desktop.interface gtk-theme"))))

(add-hook 'after-init-hook (lambda ()
			     (if (is-dark-mode)
				 (nano-dark)
			       (nano-light))))
