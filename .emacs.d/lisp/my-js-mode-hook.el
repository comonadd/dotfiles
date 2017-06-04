;;; my-js-mode-hook.el --- description
;;; Commentary:
;;; Code:

(defun my/js-mode-hook-set-style ()
  "Set the style for the JavaScript mode."
  (setq js-indent-level 2))

(defun my/js-mode-hook ()
  "The JavaScript mode hook."
  (my/js-mode-hook-set-style))

(provide 'my-js-mode-hook)
;;; my-js-mode-hook.el ends here
