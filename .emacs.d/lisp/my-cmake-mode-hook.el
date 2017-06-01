;;; my-cmake-mode-hook.el --- description
;;; Commentary:
;;; Code:

(defun my/cmake-mode-bind-keys ()
  "Bind the keys for the CMake mode."
  (local-set-key (kbd "<f9>") 'cmake-ide-compile))

(defun my/cmake-mode-hook ()
  "The CMake mode hook."
  (my/cmake-mode-bind-keys))

(provide 'my-cmake-mode-hook)
;;; my-cmake-mode-hook.el ends here
