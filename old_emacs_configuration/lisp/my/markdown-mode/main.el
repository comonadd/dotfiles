;;; main.el --- The markdown mode
;;; Commentary:
;;; Code:

(require 'flyspell)

(defun my/markdown-mode/hook ()
  "The markdown mode."
  (flyspell-mode))

(add-hook 'markdown-mode-hook 'my/markdown-mode/hook)

(provide 'my/markdown-mode/main)
;;; main.el ends here
