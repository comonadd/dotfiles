;;; hooks --- Main Emacs hooks configuration file
;;; Commentary:
;;;   This file defines all the hooks
;;; Code:

(require 'c-mode-hook)
(require 'python-mode-hook)

(add-hook 'c-mode-hook 'c-mode-hook)
(add-hook 'c++-mode-hook 'c-mode-hook)
(add-hook 'python-mode-hook 'python-mode-hook)
(add-hook 'before-save-hook 'whitespace-cleanup)

(provide 'hooks)
;;; hooks.el ends here
