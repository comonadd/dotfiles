;;; init.el --- The Emacs initialization script.
;;; Commentary:
;;; Code:

(require 'tramp)
(require 'ido)

;; Start the Emacs server
(server-start)

;; Configure tramp
(setq tramp-default-method "ssh")

;; Enable IDO mode
(ido-mode t)

;; Include other scripts
(require 'my/disable-backup)
(require 'my/disable-mods)
(require 'my/disable-tabs)
(require 'my/scroll-settings)
(require 'my/clipboard-settings)
(require 'my/gdb-settings)
(require 'my/undo-limits)
(require 'my/packages)
(require 'my/keybindings)
(require 'my/hooks)
(require 'my/encoding)
(require 'my/appearance)
(require 'my/aliases)
(require 'my/auto-insert-skeletons)
(require 'my/modes)

(provide 'my/init)
;;; init.el ends here
