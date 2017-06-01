;;; my-lisp-mode-hook -- Lisp mode hook
;;; Commentary:
;;; Code:

(defun my/lisp-mode-set-style ()
  "Set the style."
  (setq indent-tabs-mode nil)
  (setq tab-width 2))

(defun my/lisp-mode-hook ()
  "Lisp mode hook."
  (my/lisp-mode-set-style)
  (local-set-key (kbd "<f8>") 'elisp-format-buffer))

(provide 'my-lisp-mode-hook)
;;; my-lisp-mode-hook.el ends here
