;;; python-mode-main-hook --- Hook for the *.py files
;;; Commentary:
;;;   This file defines all the hooks
;;; Code:

(defun python-mode-main-hook ()
  "Python mode hook."
  (electric-indent-mode -1))

(provide 'python-mode-main-hook)
;;; python-mode-main-hook.el ends here
