;;; my/packages --- Package that setups other packages
;;; Commentary:
;;; Code:

(require 'package)

;; Update packages list
(setq package-check-signature nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

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

;; cmake-ide package
(require 'cmake-ide)
(cmake-ide-setup)

(provide 'my/packages)
;;; packages.el ends here
