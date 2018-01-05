;;; my/html-mode/main --- My HTML mode main package
;;; Commentary:
;;; Code:

(require 'emmet-mode)

(defun my/html-mode/set-style ()
  "Set the style for the HTML mode."
  (setq tab-width 2))

(defun my/html-mode/bind-keys ()
  "Bind the keys for the HTML mode."
  (local-set-key (kbd "<tab>") 'emmet-expand-yas))

(defun my/html-mode/hook ()
  "The HTML mode hook."
  (my/html-mode/set-style)
  (my/html-mode/bind-keys)
  (emmet-mode))

;; Add the hook
(add-hook 'html-mode-hook 'my/html-mode/hook)

(provide 'my/html-mode/main)
;;; main.el ends here
