;;; my/packages --- Package that setups other packages
;;; Commentary:
;;; Code:

;; Update packages list
(setq package-check-signature nil)
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(dolist (package
         '(use-package))
  (unless (package-installed-p package)
    (package-install package)))

(use-package
  smex
  :ensure t
  :init (progn
          (require 'smex)
          (smex-initialize)))

(use-package
  sublimity
  :ensure t
  :init (progn
          (require 'sublimity)
          (require 'sublimity-scroll)
          (require 'sublimity-map)
          (require 'sublimity-attractive)
          (setq sublimity-scroll-weight 10)
          (setq sublimity-scroll-drift-length 5)
          (setq sublimity-map-size 20)
          (setq sublimity-map-fraction 0.3)
          (setq sublimity-map-text-scale -4)
          (sublimity-map-set-delay 4)
          (setq sublimity-attractive-centering-width 256)
          (sublimity-attractive-hide-bars)
          (sublimity-attractive-hide-vertical-border)
          (sublimity-attractive-hide-fringes)
          (sublimity-attractive-hide-modelines)
          (sublimity-mode 1)))

(use-package
  apache-mode
  :ensure t)

(use-package
  graphql-mode
  :ensure t)

(use-package
  foggy-night-theme
  :ensure t)

;; "yaml-mode"
(use-package
  yaml-mode
  :ensure t)

;; "tidy" package
(use-package
  tidy
  :ensure t)

;; "toml-mode" package
(use-package
  toml-mode
  :ensure t)

;; "flyspell" package
(use-package
  flyspell
  :ensure t)

;; "gtags" package
(use-package
  ggtags
  :ensure t)

;; "helm-gtags" package
(use-package
  helm-gtags
  :ensure t)

;; "lua-mode" package
(use-package
  lua-mode
  :ensure t)

;; "haskell-mode" package
(use-package
  haskell-mode
  :ensure t)

;; "markdown-mode" package
(use-package
  markdown-mode
  :ensure t)

;; "magit" package
(use-package
  magit
  :ensure t)

;; "json-mode" package
(use-package
  json-mode
  :ensure t)

;; "lorem-ipsum" package
(use-package
  lorem-ipsum
  :ensure t)

;; "sass-mode" package
(use-package
  sass-mode
  :ensure t)

;; "dockerfile-mode" package
(use-package
  dockerfile-mode
  :ensure t)

;; "helm" package
(use-package
  helm
  :ensure t)

;; "clang-format" package
(use-package
  clang-format
  :ensure t)

;; "elisp-format" package
(use-package
  elisp-format
  :ensure t)

;; "ace-jump-mode" package
(use-package
  ace-jump-mode
  :ensure t)

;; "ample-theme" package
(use-package
  ample-theme
  :ensure t)

;; "string-inflection"
(use-package
  string-inflection
  :ensure t)

;; "emmet-mode" package
(use-package
  emmet-mode
  :ensure t)

;; "racer" package
(use-package
  racer
  :ensure t)

;; "undo-tree" package
(use-package
  undo-tree
  :ensure t)

;; "expand-region" package
(use-package
  expand-region
  :ensure t)

;; Linum package
(use-package
  linum
  :ensure t
  :init (progn (line-number-mode 1)
               (global-linum-mode 1)
               (column-number-mode 1)
               (setq linum-format " %d ")))

;; Multiple-Cursors package
(use-package
  multiple-cursors
  :ensure t)

;; Neotree package
(use-package
  neotree
  :ensure t
  :init (setq neo-smart-open t))

;; Flycheck package
(use-package
  flycheck
  :ensure t
  :init (progn
          (setq flycheck-gcc-language-standard "c11")
          (global-flycheck-mode)))

;; YASnippet package
(use-package
  yasnippet
  :ensure t
  :init (progn (yas-global-mode 1)
               (yas-reload-all)))

;; cmake-ide package
(use-package
  cmake-ide
  :ensure t
  :init (cmake-ide-setup))

;; indium package
(when (display-graphic-p)
  (use-package
    indium
    :ensure t))

;; jsx-mode package
(use-package
  jsx-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode)))

;; "projectile" package
(use-package
  projectile
  :ensure t)

(provide 'my/packages)
;;; packages.el ends here
