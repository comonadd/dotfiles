;;; c-mode-main-hook.el --- C mode hook
;;; Commentary:
;;;   This file defines the C mode hook
;;; Code:

(require 'projectile)

(defun my/c-mode-setup-flycheck-project-include-paths ()
  "Get the paths that we need to include in project environment."
  (let ((root (projectile-project-root)))
    (let ((c-mode-include-paths (list root (concat root "/src")
                                      (concat root "/include"))))
      (progn
        (setq flycheck-gcc-include-path c-mode-include-paths)
        (setq flycheck-clang-include-path c-mode-include-paths)))))

(defun my/c-mode-flycheck-setup ()
  "Setup the flycheck package."

  ;; Include the project paths
  (when (projectile-project-p)
    (my/c-mode-setup-flycheck-project-include-paths))

  ;; Set the flycheck additional arguments
  (let ((args (split-string "-std=c11")))
    (progn
      (setq flycheck-gcc-args args)
      (setq flycheck-clang-args args))))

(defun my/c-mode-set-style ()
  "Set the style for the C mode."
  (setq c-default-style "linux")
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (c-set-offset 'comment-intro 0)
  (c-set-offset 'case-label '+))

(defun my/c-mode-bind-keys ()
  "Bind the keys for the C mode."
  (local-set-key (kbd "<f8>") 'clang-format-buffer)
  (local-set-key (kbd "<f9>") 'cmake-ide-compile))

(defun my/c-mode-hook ()
  "C mode hook."

  ;; Disable unneeded minor modes
  (abbrev-mode -1)
  (auto-compression-mode -1)
  (auto-encryption-mode -1)
  (icomplete-mode -1)
  (mouse-wheel-mode -1)

  ;; Enable the projectile
  (projectile-mode 1)
  (when (projectile-project-p)
    (my/c-mode-flycheck-setup)
    (helm-gtags-mode 1)
    (ggtags-mode 1))

  ;; Set the style
  (my/c-mode-set-style)
  (electric-indent-mode 1)
  (my/c-mode-bind-keys))

(provide 'my-c-mode-hook)
;;; my-c-mode-hook.el ends here
