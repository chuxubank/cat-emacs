;; -*- lexical-binding: t; -*-

(straight-use-package '(request :type built-in))

(use-package netease-cloud-music
  :straight (netease-cloud-music :fork t)
  :defer t
  :custom
  (netease-cloud-music-cache-directory (concat cat-cache-dir "ncm"))
  :config
  (setq netease-cloud-music-api-type 'remote
	netease-cloud-music-api-address "https://ncm.chuxubank.vercel.app"
	netease-cloud-music-api-port "443"
	netease-cloud-music-show-lyric 'all))
