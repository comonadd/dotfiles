;;; my-python-mode-hook --- Hook for the Python mode
;;; Commentary:
;;;   This file defines all the hooks
;;; Code:

(defun my/python-mode-bind-keys ()
  "Bind keys for the Python mode."
  (local-set-key (kbd "<f8>") 'py-autopep8))

(defun my/python-mode-hook ()
  "Python mode hook."
  (electric-indent-mode -1)
  (my/python-mode-bind-keys))

(provide 'my-python-mode-hook)
;;; my-python-mode-hook.el ends here
