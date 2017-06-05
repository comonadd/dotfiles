;;; my/rust-mode/main --- My Rust mode main file
;;; Commentary:
;;; Code:

(require 'racer)
(require 'eldoc)

(defun my/rust-mode/set-style ()
  "Set the code style for the Rust mode."
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq rust-indent-offset 2)
  (setq tab-width 2)
  (defvaralias
    'c-basic-offset
    'tab-width)
  (defvaralias
    'cperl-indent-level
    'tab-width))

(defun my/rust-mode/bind-keys ()
  "Bind the keys for the Rust mode."
  (local-set-key (kbd "<f8>") 'rust-format-buffer))

(defun my/rust-mode/hook ()
  "The Rust mode hook."
  (racer-mode)
  (eldoc-mode)
  (my/rust-mode/set-style)
  (my/rust-mode/bind-keys))

;; Add the hook
(add-hook 'rust-mode-hook 'my/rust-mode/hook)

(provide 'my/rust-mode/main)
;;; main.el ends here
