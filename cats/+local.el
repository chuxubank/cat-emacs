;; -*- lexical-binding: t; -*-

(use-package calendar
  :ensure nil
  :custom
  (calendar-mark-holidays-flag t)
  (calendar-chinese-all-holidays-flag t)
  (calendar-chinese-celestial-stem ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
  (calendar-chinese-terrestrial-branch ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"]))

(use-package diary
  :ensure nil
  :custom
  (diary-file (expand-file-name "diary" cat-org-directory)))

(use-package diary-lib
  :ensure nil
  :custom
  (diary-nongregorian-listing-hook '(diary-chinese-list-entries))
  (diary-nongregorian-marking-hook '(diary-chinese-mark-entries)))

(use-package pinyin-search
  :bind
  (:map search-map
        ("p" . #'pinyin-search)))

(use-package pinyin-isearch
  :hook (isearch-mode . pinyin-isearch-activate-submodes))

(use-package ace-pinyin
  :delight
  :hook (after-init . ace-pinyin-global-mode)
  :config
  (defun avy-goto-char-timer (&optional arg)
    (interactive "P")
    (let ((avy-all-windows (if arg
                               (not avy-all-windows)
                             avy-all-windows)))
      (avy-with avy-goto-char-timer
        (setq avy--old-cands (avy--read-candidates
                              'pinyinlib-build-regexp-string))
        (avy-process avy--old-cands)))))
