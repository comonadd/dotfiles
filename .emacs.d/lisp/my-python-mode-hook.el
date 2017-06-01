;;; my-python-mode-hook --- Hook for the Python mode
;;; Commentary:
;;;   This file defines all the hooks
;;; Code:

(defun my/python-mode-hook ()
  "Python mode hook."
  (electric-indent-mode -1))

(provide 'my-python-mode-hook)
;;; my-python-mode-hook.el ends here
