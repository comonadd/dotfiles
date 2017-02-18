;;; c-mode-hook --- Main Emacs hooks configuration file
;;; Commentary:
;;;   This file defines all the C mode hook
;;; Code:

;; Navigation

(defun c-mode-include-flycheck-paths ()
  "Include the paths for flycheck."
  (let ((root (projectile-project-root)))
    (let ((src-dir-path (concat root "/src"))
          (src-include-dir-path (concat root "/src/include"))
          (include-dir-path (concat root "/include"))
          (includes-dir-path (concat root "/includes")))
      (let ((paths (list root src-dir-path src-include-dir-path include-dir-path includes-dir-path)))
        (progn
          (defvar flycheck-gcc-include-path paths)
          (defvar flycheck-clang-include-path paths))))))

(defun c-mode-set-style ()
  "Set the style."
  (setq c-default-style "linux")
  (setq c-basic-offset 4)
  (c-set-offset 'comment-intro 0)
  (c-set-offset 'case-label '+))

(defun c-mode-bind-keys ()
  "Bind the keys for the C mode."
  (defvar c-mode-map)
  (define-key c-mode-map (kbd "<f12>") 'c-mode-find-corresponding-file)
  (define-key c-mode-map (kbd "S-<f12>") 'c-mode-find-corresponding-file-other-window))

(defun c-mode-hook ()
  "C mode hook."

  ;; Disable unneeded minor modes
  (abbrev-mode -1)
  (auto-compression-mode -1)
  (auto-encryption-mode -1)
  (icomplete-mode -1)
  (mouse-wheel-mode -1)

  (c-mode-set-style)
  (projectile-mode 1)

  (when (projectile-project-p)
    (message "Including the flycheck paths")
    (c-mode-include-flycheck-paths)
    (helm-gtags-mode 1)
    (ggtags-mode 1))

  (electric-indent-mode 1)
  (c-mode-bind-keys))

(provide 'c-mode-hook)
;;; c-mode-hook.el ends here
