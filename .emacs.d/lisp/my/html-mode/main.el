;;; my/html-mode/main --- My HTML mode main package
;;; Commentary:
;;; Code:

(require 'emmet-mode)

(defun my/html-mode/set-style ()
  "Set the style for the HTML mode."
  (setq tab-width 2))

(defun my/html-mode/hook ()
  "The HTML mode hook."
  (my/html-mode/set-style)
  (emmet-mode))

(provide 'my/html-mode/main)
;;; main.el ends here
