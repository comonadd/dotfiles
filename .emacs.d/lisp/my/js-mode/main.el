;;; my/js-mode/main.el --- description
;;; Commentary:
;;; Code:

(require 'js-auto-beautify)
(require 'web-beautify)
(require 'js2-mode)

(defun my/js-mode/set-style ()
  "Set the style for the JavaScript mode."
  (setq js-indent-level 2))

(defun my/js-mode/bind-keys ()
  "Bind keys for the JavaScript mode."
  (local-set-key (kbd "<f8>") 'web-beautify-js))

(defun my/js-mode/add-hooks ()
  "Add hooks for the JavaScript mode."
  (add-hook 'before-save-hook 'web-beautify-js nil 'make-it-local))

(defun my/js-mode/hook ()
  "The JavaScript mode hook."
  (my/js-mode/set-style)
  (my/js-mode/bind-keys)
  (my/js-mode/add-hooks)
  (js-auto-beautify-mode)
  (setq js2-highlight-level 3)
  (js2-mode)
  (flycheck-mode t))

;; Add the hook
(add-hook 'js-mode-hook 'my/js-mode/hook)

(provide 'my/js-mode/main)
;;; main.el ends here
