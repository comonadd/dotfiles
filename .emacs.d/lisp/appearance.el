;; Basics
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Fonts
(set-face-attribute
 'default nil
 :family "Consolas"
 :height 120
 :weight 'normal
 :width 'normal)

;; Load theme
(load (concat my/config-root "whatever-theme.el"))
(load-theme 'whatever t)
;;(load-theme 'doom-city-lights t)
