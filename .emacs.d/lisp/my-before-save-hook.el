;;; my-before-save-hook.el --- description
;;; Commentary:
;;; Code:

(defun my/before-save-hook ()
  "The before save hook."
  (whitespace-cleanup))

(provide 'my-before-save-hook)
;;; my-before-save-hook.el ends here
