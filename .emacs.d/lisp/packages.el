;;; Packages --- Packages initialization
;;; Commentary:
;;;   This file handles all the installed packages
;;; Code:

(require 'package)
(setq package-check-signature nil)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(require 'use-package)

;; Autopair package
(use-package autopair
  :init (autopair-global-mode))

;; EDE package
(use-package ede
  :init (global-ede-mode t))

;; HS package
(use-package hideshow
  :bind (("<f9>" . hs-toggle-hiding)
         ("C-<f9>" . hs-hide-all)
         ("C-S-<f9>" . hs-show-all)))

;; Helm package
(use-package helm-gtags
  :init (setq
          helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-prefix-key "\C-cg"
          helm-gtags-suggested-key-mapping t))

;; Imenu Package
(use-package imenu
    :bind ("<f6>" . imenu)
    :init (setq imenu-auto-rescan t)
          (setq imenu-use-popup-menu nil))

;; IDO Package
(use-package ido
    :init
    (ido-mode t)
    (icomplete-mode t)
    ;; (ido-everywhere t)
    (setq ido-virtual-buffers t)
    (setq ido-enable-flex-matching t))

;; Linum Package
(use-package linum
    :init
    (line-number-mode t)
    (global-linum-mode t)
    (column-number-mode t)
    (setq linum-format " %d"))

;; Multiple-Cursors Package
(use-package multiple-cursors
    :bind (("C-." . mc/mark-next-like-this)
           ("C-," . mc/mark-previous-like-this))
    :init)

;; Projectile package
(use-package projectile)

;; Flycheck Package
(use-package flycheck
    :init
    (setq flycheck-gcc-language-standard "c11")
    (global-flycheck-mode))

;; SRefactor Package
(use-package srefactor
    :init
    (semantic-mode 1))

(provide 'packages)
;;; packages.el ends here
