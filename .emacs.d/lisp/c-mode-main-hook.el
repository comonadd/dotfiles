;;; c-mode-main-hook.el --- C mode hook
;;; Commentary:
;;;   This file defines the C mode hook
;;; Code:

;;;;;;;;;;
;; Main ;;
;;;;;;;;;;

(defvar flycheck-gcc-args)
(defvar flycheck-clang-args)
(defvar flycheck-gcc-include-path)
(defvar flycheck-clang-include-path)

(defun c-mode-setup-flycheck-project-include-paths ()
  "Get the paths that we need to include in project environment."
  (let ((root (projectile-project-root)))
    (let ((c-mode-include-paths (list root (concat root "/src") (concat root "/include"))))
      (progn
        (setq flycheck-gcc-include-path c-mode-include-paths)
        (setq flycheck-clang-include-path c-mode-include-paths)))))

(defun c-mode-flycheck-setup ()
  "Setup the flycheck package."

  ;; Include the project paths
  (when (projectile-project-p)
    (c-mode-setup-flycheck-project-include-paths))

  ;; Set the flycheck additional arguments
  (let ((args (split-string
               (concat (shell-command-to-string "pkg-config --cflags --libs gtk+-3.0")
                       "-std=c11"))))
    (progn
      (setq flycheck-gcc-args args)
      (setq flycheck-clang-args args))))

(defun c-mode-set-style ()
  "Set the style."
  (setq c-default-style "linux")
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (c-set-offset 'comment-intro 0)
  (c-set-offset 'case-label '+))

(defun c-mode-bind-keys ()
  "Bind the keys for the C mode."
  (defvar c-mode-map))

(defun c-mode-main-hook ()
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
    (c-mode-flycheck-setup)
    (helm-gtags-mode 1)
    (ggtags-mode 1))

  ;; Set the style
  (c-mode-set-style)

  (electric-indent-mode 1)
  (c-mode-bind-keys))

(provide 'c-mode-main-hook)
;;; c-mode-main-hook.el ends here
