;;; my/lisp-mode/main -- Main package of my Lisp mode
;;; Commentary:
;;; Code:

(defun my/lisp-mode/set-style ()
  "Set the style for the Lisp mode."
  (setq indent-tabs-mode nil)
  (setq tab-width 2))

(defun my/lisp-mode/hook ()
  "Lisp mode hook."
  (my/lisp-mode/set-style)
  (local-set-key (kbd "<f8>") 'elisp-format-buffer))

;; Add the hooks
(add-hook 'lisp-mode-hook 'my/lisp-mode/hook)
(add-hook 'emacs-lisp-mode-hook 'my/lisp-mode/hook)

(provide 'my/lisp-mode/main)
;;; main.el ends here
