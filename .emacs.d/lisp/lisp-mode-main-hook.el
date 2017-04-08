;;; lisp-mode-main-hook.el --- Lisp mode hook
;;; Commentary:
;;; Code:

(defun lisp-mode-set-style ()
  "Set the style."
  (setq indent-tabs-mode nil)
  (setq tab-width 2))

(defun lisp-mode-main-hook ()
  "Lisp mode hook."
  (lisp-mode-set-style))

(provide 'lisp-mode-main-hook)
;;; lisp-mode-main-hook.el ends here
