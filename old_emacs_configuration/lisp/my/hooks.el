;;; my/hooks --- Package that defines some hooks
;;; Commentary:
;;; Code:
;;

;; Other hooks
(require 'my/before-save-hook)
(add-hook 'before-save-hook 'my/before-save-hook)

(provide 'my/hooks)
;;; hooks.el ends here
