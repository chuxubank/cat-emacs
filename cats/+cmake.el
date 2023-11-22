;; -*- lexical-binding: t; -*-

(use-package cmake-mode
  :ensure-system-package
  (cmake . cmake)
  (cmake-format . "pip install cmake-format")
  (cmake-language-server . "pip install cmake-language-server"))

(use-package cmake-font-lock)
