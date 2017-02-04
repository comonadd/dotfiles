;;; python-mode-hook --- Hook for the *.py files
;;; Commentary:
;;;   This file defines all the hooks
;;; Code:

(defun python-mode-hook ()
  "Python mode hook."
  (electric-indent-mode -1))

(provide 'python-mode-hook)
;;; python-mode-hook.el ends here
