;;; my/cmake-mode/main --- description
;;; Commentary:
;;; Code:

(require 'cmake-ide)

(defun my/cmake-mode/bind-keys ()
  "Bind the keys for the CMake mode."
  (local-set-key (kbd "<f9>") 'cmake-ide-compile))

(defun my/cmake-mode/hook ()
  "The CMake mode hook."
  (my/cmake-mode/bind-keys))

(provide 'my/cmake-mode/main)
;;; main.el ends here
