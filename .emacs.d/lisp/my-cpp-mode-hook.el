;;; my-cpp-mode-hook.el --- description
;;; Commentary:
;;; Code:

(require 'projectile)

(defun my/c++-mode-setup-flycheck-project-include-paths ()
  "Get the paths that we need to include in project environment."
  (let ((root (projectile-project-root)))
    (let ((include-paths (list root (concat root "/src")
                               (concat root "/include"))))
      (progn
        (setq flycheck-gcc-include-path include-paths)
        (setq flycheck-clang-include-path include-paths)))))

(defun my/c++-mode-flycheck-setup ()
  "Setup the flycheck package."
  (setq-default flycheck-disabled-checkers '(c/c++-clang))

  ;; Include the project paths
  (when (projectile-project-p)
    (my/c++-mode-setup-flycheck-project-include-paths))

  ;; Set the flycheck additional arguments
  (let ((args (split-string "-std=c++17")))
    (progn
      (setq flycheck-gcc-args args)
      (setq flycheck-clang-args args))))

(defun my/c++-mode-set-style ()
  "Set the style."
  (c-add-style "custom" '("bsd" (indent-tabs-mode . nil)
                          (c-basic-offset . 2)
                          (tab-width . 2)
                          (c-offsets-alist . ((comment-intro . 0)
                                              (case-label . +)))))
  (c-set-style "custom"))

(defun my/c++-mode-bind-keys ()
  "Bind the keys for the C mode."
  (local-set-key (kbd "<f8>") 'clang-format-buffer)
  (local-set-key (kbd "<f9>") 'cmake-ide-compile))

(defun my/c++-mode-hook ()
  "C++ mode hook."

  ;; Disable unneeded minor modes
  (abbrev-mode -1)
  (auto-compression-mode -1)
  (auto-encryption-mode -1)
  (icomplete-mode -1)
  (mouse-wheel-mode -1)

  ;; Enable the projectile
  (projectile-mode 1)
  (my/c++-mode-flycheck-setup)
  (when (projectile-project-p)
    (helm-gtags-mode 1)
    (ggtags-mode 1))

  ;; Set the style
  (my/c++-mode-set-style)
  (electric-indent-mode 1)
  (my/c++-mode-bind-keys))

(provide 'my-cpp-mode-hook)
;;; my-cpp-mode-hook.el ends here
