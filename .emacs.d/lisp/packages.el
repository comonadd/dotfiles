;;; Packages --- Packages initialization
;;; Commentary:
;;;   This file handles all the installed packages
;;; Code:

(require 'package)
(setq package-check-signature nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Ergoemacs Mode
;;(require 'ergoemacs-mode)
;;(setq ergoemacs-theme nil)
;;(setq ergoemacs-keyboard-layout "us")
;;(ergoemacs-mode 1)

;; Linum package
(require 'linum)
(line-number-mode 1)
(global-linum-mode 1)
(column-number-mode 1)
(setq linum-format " %d ")

;; Multiple-Cursors package
(require 'multiple-cursors)

;; Neotree package
(require 'neotree)
(setq neo-smart-open t)
;; (setq projectile-switch-project-action 'neotree-projectile-action)

;; Flycheck package
(require 'flycheck)
(setq flycheck-gcc-language-standard "c11")
(global-flycheck-mode)

;; YASnippet package
(require 'yasnippet)
(yas-global-mode)

;; Helm package
(require 'helm)

;; RTags package
;; (require 'rtags)
;; (require 'company-rtags)

;; (setq rtags-completions-enabled t)
;; (eval-after-load 'company
;;   '(add-to-list
;;     'company-backends 'company-rtags))
;; (setq rtags-autostart-diagnostics t)
;; (rtags-enable-standard-keybindings)

;; cmake-ide package
(require 'cmake-ide)
(cmake-ide-setup)

;; Projectile package
;(require 'projectile)

;; Autopair package
;; (use-package autopair
;;   :init (autopair-global-mode))

;(use-package ergoemacs-mode
  ;:init (setq ergoemacs-theme nil)
        ;(setq ergoemacs-keyboard-layout "us")
        ;(ergoemacs-mode 1))

;; EDE package
;(use-package ede
  ;:init (global-ede-mode t))

;; Sublimity package
;(use-package sublimity
  ;:init
  ;(sublimity-mode 1)
  ;(setq sublimity-map-size 20)
  ;(setq sublimity-map-fraction 0.3)
  ;(setq sublimity-map-text-scale -7))
;(use-package sublimity-map)

;; HS package
;; (use-package hideshow
;;   :bind (("<f9>" . hs-toggle-hiding)
;;          ("C-<f9>" . hs-hide-all)
;;          ("C-S-<f9>" . hs-show-all)))

;; Helm package
;(use-package helm-gtags
  ;:init (setq
          ;helm-gtags-ignore-case t
          ;helm-gtags-auto-update t
          ;helm-gtags-use-input-at-cursor t
          ;helm-gtags-pulse-at-cursor t
          ;helm-gtags-prefix-key "\C-cg"
          ;helm-gtags-suggested-key-mapping t))

;; Imenu Package
;(use-package imenu
    ;:bind ("<f6>" . imenu)
    ;:init (setq imenu-auto-rescan t)
          ;(setq imenu-use-popup-menu nil))

;; IDO Package
;(use-package ido
    ;:init
    ;(ido-mode t)
    ;(icomplete-mode t)
    ; (ido-everywhere t)
    ;(setq ido-virtual-buffers t)
    ;(setq ido-enable-flex-matching t))

;; SRefactor Package
;; (use-package srefactor
;;     :init
;;     (semantic-mode 1))

(provide 'packages)
;;; packages.el ends here
