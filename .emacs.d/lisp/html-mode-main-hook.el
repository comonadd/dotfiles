;;; html-mode-main-hook.el --- description
;;; Commentary:
;;; Code:


(defun html-mode-set-style ()
  "Set the style for the HTML mode"
  (setq tab-width 2))

(defun html-mode-main-hook ()
  "The HTML mode main hook"
  (html-mode-set-style))

(provide 'html-mode-main-hook)
;;; html-mode-main-hook.el ends here
