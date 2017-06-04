;;; my/js-mode/main.el --- description
;;; Commentary:
;;; Code:

(defun my/js-mode/set-style ()
  "Set the style for the JavaScript mode."
  (setq js-indent-level 2))

(defun my/js-mode/hook ()
  "The JavaScript mode hook."
  (my/js-mode/set-style))

(provide 'my/js-mode/main)
;;; main.el ends here
