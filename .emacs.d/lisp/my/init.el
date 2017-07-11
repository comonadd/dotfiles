;;; init.el --- The Emacs initialization script.
;;; Commentary:
;;; Code:

(require 'tramp)

;; Start the Emacs server
(server-start)

(setq tramp-default-method "ssh")

;; Include other scripts
(require 'my/disable-backup)
(require 'my/disable-mods)
(require 'my/disable-tabs)
(require 'my/scroll-settings)
(require 'my/clipboard-settings)
(require 'my/gdb-settings)
(require 'my/undo-limits)
(require 'my/appearance)
(require 'my/packages)
(require 'my/keybindings)
(require 'my/hooks)
(require 'my/encoding)
(require 'my/aliases)
(require 'my/auto-insert-skeletons)
(require 'my/modes)

(provide 'my/init)
;;; init.el ends here
