;;; my-html-mode-hook --- The HTML mode hook
;;; Commentary:
;;; Code:

(defun my/html-mode-set-style ()
  "Set the style for the HTML mode."
  (setq tab-width 2))

(defun my/html-mode-hook ()
  "The HTML mode main hook."
  (my/html-mode-set-style)
  (emmet-mode))

(provide 'my-html-mode-hook)
;;; my-html-mode-hook.el ends here
