;;; my/js-mode/main.el --- description
;;; Commentary:
;;; Code:

(require 'flycheck)
(require 'js-auto-beautify)
(require 'web-beautify)
(require 'js2-mode)
(require 'web-mode)

(defun my/js-mode/set-style ()
  "Set the style for the JavaScript mode."
  (setq js-indent-level 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(defun my/js-mode/bind-keys ()
  "Bind keys for the JavaScript mode."
  (local-set-key (kbd "<f8>")
                 (lambda ()
                   (interactive)
                   (indent-region (point-min)
                                  (point-max)))))

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
  (flycheck-mode t)
  (web-mode))

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil)) ad-do-it) ad-do-it))

;; Add the hooks
(add-hook 'js-mode-hook 'my/js-mode/hook)
(add-hook 'js2-mode-hook 'my/js-mode/hook)
(add-hook 'web-mode-hook (lambda ()
                           ;; short circuit js mode and just do everything in jsx-mode
                           (if (equal web-mode-content-type "javascript")
                               (web-mode-set-content-type "jsx")
                             (message "now set to: %s" web-mode-content-type))))

(provide 'my/js-mode/main)
;;; main.el ends here
