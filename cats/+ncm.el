;; -*- lexical-binding: t; -*-

(use-package netease-cloud-music
  :vc (netease-cloud-music
       :url "https://github.com/chuxubank/netease-cloud-music.el"
       :rev :newest)
  :custom
  (netease-cloud-music-cache-directory (concat cat-cache-dir "ncm"))
  :config
  (setq netease-cloud-music-api-type 'remote
        netease-cloud-music-api-address "https://ncm.chuxubank.vercel.app"
        netease-cloud-music-api-port "443"
        netease-cloud-music-show-lyric 'all))
