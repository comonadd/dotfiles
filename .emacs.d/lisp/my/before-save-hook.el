;;; my/before-save-hook --- My "before save" hook
;;; Commentary:
;;; Code:

(defun my/before-save-hook ()
  "\"Before save\" hook."
  (whitespace-cleanup))

(provide 'my/before-save-hook)
;;; before-save-hook.el ends here
