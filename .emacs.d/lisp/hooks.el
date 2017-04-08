;;; hooks --- Main Emacs hooks configuration file
;;; Commentary:
;;;   This file defines all the hooks
;;; Code:

(require 'c-mode-main-hook)
(require 'cpp-mode-main-hook)
(require 'python-mode-main-hook)
(require 'lisp-mode-main-hook)
(require 'html-mode-main-hook)

;; Mode hooks
(add-hook 'c-mode-hook 'c-mode-main-hook)
(add-hook 'c++-mode-hook 'c++-mode-main-hook)
(add-hook 'python-mode-hook 'python-mode-main-hook)
(add-hook 'lisp-mode-hook 'lisp-mode-main-hook)
(add-hook 'emacs-lisp-mode-hook 'lisp-mode-main-hook)
(add-hook 'html-mode-hook 'html-mode-main-hook)

;; Other hooks
(add-hook 'before-save-hook 'whitespace-cleanup)

(provide 'hooks)
;;; hooks.el ends here
