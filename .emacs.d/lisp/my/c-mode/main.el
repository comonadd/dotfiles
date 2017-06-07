;;; my/c-mode/main.el --- C mode hook
;;; Commentary:
;;; Code:

(require 'projectile)

(defun my/c-mode-find-corresponding-file ()
  "Find the file that corresponds to this one
(header file to source file or vice-versa),
and open it in current window."
  (interactive)
  (let ((corresponding-file-name) (base-file-name (buffer-get-base-file-name)))
    (progn
      (cond
       ((string-match "\\.c" buffer-file-name)
        (setq corresponding-file-name (concat base-file-name ".h")))
       ((string-match "\\.h" buffer-file-name)
        (setq corresponding-file-name (concat base-file-name ".c"))))
(if corresponding-file-name (find-file corresponding-file-name)
  (error "Unable to find a corresponding C file")))))

(defun my/c-mode-find-corresponding-file-other-window ()
  "Execute `my/c-mode-find-corresponding-file` in other window."
  (interactive)
  (find-file-other-window buffer-file-name)
  (my/c-mode-find-corresponding-file)
  (other-window -1))

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
  (local-set-key (kbd "<f9>") 'cmake-ide-compile)
  (local-set-key (kbd "<f12>") 'my/c-mode-find-corresponding-file)
  (local-set-key (kbd "S-<f12>") 'my/c-mode-find-corresponding-file-other-window))

(defun my/c-mode-hook ()
  "C mode hook."

  ;; Enable/disable minor modes
  (abbrev-mode -1)
  (auto-compression-mode -1)
  (auto-encryption-mode -1)
  (icomplete-mode -1)
  (mouse-wheel-mode -1)
  (subword-mode)

  ;; Enable the projectile
  (projectile-mode 1)
  (when (projectile-project-p)
    (my/c-mode-flycheck-setup)
    (helm-gtags-mode 1)
    (ggtags-mode 1))


  (my/c-mode-set-style)
  (electric-indent-mode 1)
  (my/c-mode-bind-keys))

;; Add the hook
(add-hook 'c-mode-hook 'my/c-mode/hook)

(provide 'my/c-mode/main)
;;; main.el ends here
