;;; my/packages --- Package that setups other packages
;;; Commentary:
;;; Code:

(require 'package)

;; Update packages list
(setq package-check-signature nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(global-subword-mode)

;; Custom modes
(require 'my/c-mode/main)
(require 'my/cpp-mode/main)
(require 'my/html-mode/main)
(require 'my/rust-mode/main)
(require 'my/lisp-mode/main)
(require 'my/cmake-mode/main)
(require 'my/python-mode/main)
(require 'my/js-mode/main)
(require 'my/css-mode/main)
(require 'my/ebnf-mode)

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
(yas-global-mode 1)
(yas-reload-all)

;; cmake-ide package
(require 'cmake-ide)
(cmake-ide-setup)

;; indium package
(when (display-graphic-p)
  (require 'indium))

;; jsx-mode package
(require 'jsx-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))

(provide 'my/packages)
;;; packages.el ends here
