;;
;;; Global variables
(setq-default my/user-root (file-name-as-directory (getenv "HOME")))
(setq-default my/emacs-root (file-name-as-directory (concat my/user-root ".emacs.d")))
(setq-default my/config-root (file-name-as-directory (concat my/user-root ".emacs.d/lisp")))
(setq-default my/config-file (concat my/emacs-root "init.el"))
(setq-default my/projects-root (concat my/user-root "Projects"))

;;
;;; Initialize the package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;
;;; Path configuration
(add-to-list 'load-path (expand-file-name my/config-root))

;;
;;; Require other configuration parts
(load "basics.el")
(load "functions.el")
(load "packages.el")
(load "modes.el")
(load "keybindings.el")
(load "appearance.el")

;;
;;; Auto-generated stuff

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3273d17945773c9000304645a888ca882c7ed69afb8ca761f6f830dca1788b26" default))
 '(initial-frame-alist '((fullscreen . maximized)))
 '(package-selected-packages
   '(lsp-ui rust-mode treemacs-all-the-icons treemacs-projectile treemacs-evil doom-themes lsp-treemacs all-the-icons zzz-to-char company-box elisp-format lsp-ivy company projectile evil-nerd-commenter bm evil-mc evil-magit magit counsel evil-collection use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
