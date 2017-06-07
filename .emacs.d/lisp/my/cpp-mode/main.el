;;; my/cpp-mode/main --- The main file of my C++ mode
;;; Commentary:
;;; Code:

(require 'projectile)
(require 'my/cpp-mode/util)

(defun my/cpp-mode/hook ()
  "C++ mode hook."

  ;; Disable unneeded minor modes
  (abbrev-mode -1)
  (auto-compression-mode -1)
  (auto-encryption-mode -1)
  (icomplete-mode -1)
  (mouse-wheel-mode -1)

  ;; Enable the projectile
  (projectile-mode 1)
  (my/cpp-mode-flycheck-setup)
  (when (projectile-project-p)
    (helm-gtags-mode 1)
    (ggtags-mode 1))

  ;; Set the style
  (my/cpp-mode-set-style)
  (electric-indent-mode 1)
  (my/cpp-mode-bind-keys))

;; Add the hook
(add-hook 'c++-mode-hook 'my/cpp-mode/hook)

(provide 'my/cpp-mode/main)
;;; main.el ends here
