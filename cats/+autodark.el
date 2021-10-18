(defun cat-is-dark-mode()
  (cond
   ((or IS-WSL IS-WINDOWS) (string-equal "0" (string-trim (shell-command-to-string "powershell.exe -WindowStyle Hidden -C Get-ItemPropertyValue -Path HKCU://Software/Microsoft/Windows/CurrentVersion/Themes/Personalize -Name AppsUseLightTheme"))))
   (IS-LINUX   (string-match-p "-dark" (shell-command-to-string "gsettings get org.gnome.desktop.interface gtk-theme")))
   (IS-MAC     (string-match-p "Dark" (shell-command-to-string "defaults read -g AppleInterfaceStyle")))))

(add-hook 'after-init-hook (lambda ()
			     (if (cat-is-dark-mode)
				 (nano-dark)
			       (nano-light))))
