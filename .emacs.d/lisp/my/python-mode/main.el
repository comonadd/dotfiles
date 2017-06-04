;;; my/python-mode/main --- description
;;; Commentary:
;;; Code:

(defun my/python-mode/bind-keys ()
  "Bind keys for the Python mode."
  (local-set-key (kbd "<f8>") 'py-autopep8))

(defun my/python-mode/hook ()
  "Python mode hook."
  (electric-indent-mode -1)
  (my/python-mode/bind-keys))

(provide 'my/python-mode/main)
;;; main.el ends here
